"use strict";


const Ops = {
  LEFT: 0,
  RIGHT: 1,
  ADD: 2,
  SUB: 3,
  LOOP_START: 4,
  LOOP_END: 5,
  OUTPUT: 6,
  INPUT: 7,
  ZERO: 8,
  MULTIPLY: 9,
};

class Op {
  constructor(type, arg = 0, offset = 0) {
    this.type = type;
    this.arg = arg;
    this.offset = offset;
  }
}

/* Contraction helper for create_program */
const consec_op = (bytes, i, type) => {
  const start = i;
  while (i < bytes.length && bytes[i] === type) i++;
  const count = i - start;
  return [i - 1, count];
};

const create_program = (bytes) => {
  const prog = [];
  let i = 0;
  while (i < bytes.length) {
    let count = 0;
    switch (bytes[i]) {
      case 60: [i, count] = consec_op(bytes, i, 60); prog.push(new Op(Ops.LEFT, count)); break;
      case 62: [i, count] = consec_op(bytes, i, 62); prog.push(new Op(Ops.RIGHT, count)); break;
      case 43: [i, count] = consec_op(bytes, i, 43); prog.push(new Op(Ops.ADD, count)); break;
      case 45: [i, count] = consec_op(bytes, i, 45); prog.push(new Op(Ops.SUB, count)); break;
      case 91: prog.push(new Op(Ops.LOOP_START)); break;
      case 93: prog.push(new Op(Ops.LOOP_END)); break;
      case 46: prog.push(new Op(Ops.OUTPUT)); break;
      case 44: prog.push(new Op(Ops.INPUT)); break;
      default: break;
    }
    i++;
  }
  return prog;
};

/* Net opposing op counts */
const optimize_contraction = (prog) => {
  const out = [];
  for (const op of prog) {
    // handle data ops (+/-)
    if (op.type === Ops.ADD || op.type === Ops.SUB) {
      // signed count: + for ADD, – for SUB
      let net = (op.type === Ops.ADD ? +1 : -1) * op.arg;
      // if previous was also ADD/SUB, absorb it
      const last = out[out.length - 1];
      if (last && (last.type === Ops.ADD || last.type === Ops.SUB)) {
        out.pop();
        net += (last.type === Ops.ADD ? +1 : -1) * last.arg;
      }
      // only emit if nonzero
      if (net > 0)    out.push(new Op(Ops.ADD, net));
      else if (net < 0) out.push(new Op(Ops.SUB, -net));
      // if net === 0, they cancel entirely
    }
    // handle pointer ops (<, >)
    else if (op.type === Ops.LEFT || op.type === Ops.RIGHT) {
      let net = (op.type === Ops.RIGHT ? +1 : -1) * op.arg;
      const last = out[out.length - 1];
      if (last && (last.type === Ops.LEFT || last.type === Ops.RIGHT)) {
        out.pop();
        net += (last.type === Ops.RIGHT ? +1 : -1) * last.arg;
      }
      if (net > 0)    out.push(new Op(Ops.RIGHT, net));
      else if (net < 0) out.push(new Op(Ops.LEFT, -net));
    }
    // everything else passes through
    else {
      out.push(op);
    }
  }
  return out;
};



const optimize_clear_loop = (prog) => {
  for (let i = 0; i < prog.length - 2; i++) {
    if (
      prog[i].type === Ops.LOOP_START &&
      ((prog[i + 1].type === Ops.SUB && prog[i + 1].arg === 1) ||
        (prog[i + 1].type === Ops.ADD && prog[i + 1].arg === 1)) &&
      prog[i + 2].type === Ops.LOOP_END
    ) {
      prog.splice(i, 3, new Op(Ops.ZERO));
    }
  }
};


const optimize_copy_loop = (prog) => {
  for (let i = 0; i < prog.length; i++) {
    if (prog[i].type !== Ops.LOOP_START) continue;
    let j = i + 1;
    let pos = 0;
    const copies = [];
    if (!(prog[j]?.type === Ops.SUB && prog[j].arg === 1)) continue;
    j++;
    for (; j < prog.length && prog[j].type !== Ops.LOOP_END; j++) {
      const op = prog[j];
      if (op.type === Ops.RIGHT) pos += op.arg;
      else if (op.type === Ops.LEFT) pos -= op.arg;
      else if (op.type === Ops.ADD) copies.push({ offset: pos, mult: op.arg });
      else break;
    }
    if (pos === 0 && prog[j]?.type === Ops.LOOP_END) {
      const newOps = copies.map(c => new Op(Ops.MULTIPLY, c.mult, c.offset));
      newOps.push(new Op(Ops.ZERO));
      prog.splice(i, j - i + 1, ...newOps);
      i--;
    }
  }
};


const optimize_multiplication_loops = (prog) => {
  outer: for (let i = 0; i < prog.length; i++) {
    if (prog[i].type !== Ops.LOOP_START) continue;

    let j = i + 1;
    let ptrDelta = 0;
    const mult = new Map();

    if (!(prog[j] && prog[j].type === Ops.SUB && prog[j].arg === 1)) continue;
    j++;

    while (j < prog.length && prog[j].type !== Ops.LOOP_END) {
      const op = prog[j];

      switch (op.type) {
        case Ops.LEFT:
          ptrDelta -= op.arg;
          break;

        case Ops.RIGHT:
          ptrDelta += op.arg;
          break;

        case Ops.ADD:
        case Ops.SUB: {
          const off = ptrDelta;
          const delta = (op.type === Ops.ADD ? +op.arg : -op.arg);
          mult.set(off, (mult.get(off) || 0) + delta);
          break;
        }

        default:
          continue outer;
      }
      j++;
    }

    if (prog[j]?.type !== Ops.LOOP_END || ptrDelta !== 0) continue;

    const newOps = [];
    for (const [off, m] of mult.entries()) {
      if (off !== 0 && m !== 0)
        newOps.push(new Op(Ops.MULTIPLY, m, off));
    }
    newOps.push(new Op(Ops.ZERO));

    prog.splice(i, j - i + 1, ...newOps);
    i--;
  }
};



const optimize_offsets = (prog) => {
  const out = [];
  let acc = 0;

  const emitMove = () => {
    if (acc === 0) return;
    out.push(new Op(acc > 0 ? Ops.RIGHT : Ops.LEFT, Math.abs(acc)));
    acc = 0;
  };

  const CAN_OFFSET = new Set([Ops.ADD, Ops.SUB, Ops.ZERO, Ops.OUTPUT, Ops.INPUT]);

  for (const op of prog) {
    switch (op.type) {
      case Ops.LEFT:
        acc -= op.arg;
        break;
      case Ops.RIGHT:
        acc += op.arg;
        break;

      default:
        if (CAN_OFFSET.has(op.type)) {
          out.push(new Op(op.type, op.arg, op.offset + acc));
          break;
        }

        emitMove();
        out.push(op);
    }


    if (op.type === Ops.LOOP_START) emitMove();
  }

  emitMove();
  return out;
};




const align_brackets = (prog) => {
  const stack = [];
  prog.forEach((op, i) => {
    if (op.type === Ops.LOOP_START) stack.push(i);
    else if (op.type === Ops.LOOP_END) {
      const open = stack.pop();
      prog[open].arg = i;
      prog[i].arg = open;
    }
  });
};


const bf_eval = (prog) => {
  const cells = new Uint32Array(1 << 16);
  let cc = 0, pc = 0;
  while (pc < prog.length) {
    const op = prog[pc++];
    switch (op.type) {
      case Ops.LEFT:
        cc -= op.arg;
        break;
      case Ops.RIGHT:
        cc += op.arg;
        break;
      case Ops.ADD:
        cells[cc + op.offset] += op.arg;
        break;
      case Ops.SUB:
        cells[cc + op.offset] -= op.arg;
        break;
      case Ops.ZERO:
        cells[cc + op.offset] = 0;
        break;
      case Ops.LOOP_START:
        if (!cells[cc]) pc = op.arg;
        break;
      case Ops.LOOP_END:
        if (cells[cc]) pc = op.arg;
        break;
      case Ops.MULTIPLY:
        cells[cc + op.offset] += cells[cc] * op.arg;
        break;
      case Ops.OUTPUT:
        process.stdout.write(String.fromCharCode(cells[cc + op.offset]));
        break;
      case Ops.INPUT:
        cells[cc + op.offset] = fs.readSync(
          process.stdin.fd,
          Buffer.alloc(1),
          0,
          1,
          null
        )[0];
        break;
    }
  }
};




// build a reverse lookup from code → name
const OpName = Object.entries(Ops)
  .reduce((m, [name, code]) => { m[code] = name; return m; }, {});

function dumpProg(label, prog) {
  console.log(`\n=== ${label} (${prog.length} ops) ===`);
  console.log(
    prog
      .map(op =>
        op.offset
          ? `${OpName[op.type]}(${op.arg}@${op.offset})`
          : `${OpName[op.type]}(${op.arg})`
      )
      .join('  ')
  );
}






export const run = (bytes) => {
  let prog = create_program(bytes);
  //dumpProg('after create_program', prog);

  prog = optimize_contraction(prog);
  //dumpProg('after optimize_contraction', prog);

  optimize_multiplication_loops(prog);
  //dumpProg('after multiplication loops', prog);


  prog = optimize_contraction(prog);
  //dumpProg('after optimize_contraction', prog);

  optimize_clear_loop(prog);
  //dumpProg('after clear loops', prog);


  
  prog = optimize_contraction(prog);
  //dumpProg('after optimize_contraction', prog);

  prog = optimize_offsets(prog);
  //dumpProg('after optimize_offsets', prog);


  align_brackets(prog);
  //dumpProg('after align_brackets', prog);

  bf_eval(prog);
};
