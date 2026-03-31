"use strict";
// ES imports require node 22
import fs from "fs";

const hello = fs.readFileSync("bf/hello.bf");
const bench = fs.readFileSync("bf/bench.bf");
const hanoi = fs.readFileSync("bf/hanoi.bf");
const mande = fs.readFileSync("bf/mandelbrot.bf");

import { run as interp1 } from "./step-1/interp.js";
import { run as interp2 } from "./step-2/interp.js";
import { run as interp3 } from "./step-3/interp.js";
import { run as interp4 } from "./step-4/interp.js";
import { run as interp5 } from "./step-5/interp.js";
import { run as interp6 } from "./step-6/interp.js";
import { run as interp7 } from "./step-7/interp.js";

let benchmark = function (method, iterations, bytes) {
  let time = 0;
  let timer = function (action) {
    let d = Date.now();
    if (time < 1 || action === "start") {
      time = d;
      return 0;
    } else if (action === "stop") {
      let t = d - time;
      time = 0;
      return t;
    } else {
      return d - time;
    }
  };

  let i = 0;
  timer("start");
  while (i < iterations) {
    method(bytes);
    i++;
  }

  return timer("stop");
};

let marks = {};
marks["1: hello"] = benchmark(interp1, 100, hello);
marks["2: hello"] = benchmark(interp2, 100, hello);
marks["3: hello"] = benchmark(interp3, 100, hello);
marks["4: hello"] = benchmark(interp4, 100, hello);
marks["5: hello"] = benchmark(interp5, 100, hello);
marks["6: hello"] = benchmark(interp6, 100, hello);
marks["7: hello"] = benchmark(interp7, 100, hello);

marks["1: bench"] = benchmark(interp1, 5, bench);
marks["7: bench"] = benchmark(interp7, 5, bench);

marks["1: hanoi"] = benchmark(interp1, 1, hanoi);
marks["7: hanoi"] = benchmark(interp7, 1, hanoi);

marks["1: mandel"] = benchmark(interp1, 1, mande);
marks["7: mandel"] = benchmark(interp7, 1, mande);

console.log(marks);
