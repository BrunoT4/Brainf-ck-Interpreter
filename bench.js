"use strict";
// ES imports require node 22
import fs from "fs";

const hello = fs.readFileSync("bf/hello.bf");
const bench = fs.readFileSync("bf/bench.bf");
const hanoi = fs.readFileSync("bf/hanoi.bf");
const mande = fs.readFileSync("bf/mandelbrot.bf");
import { run as interp } from "./interp.js";

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
marks["1: hello"] = benchmark(interp, 100, hello);

marks["2: bench"] = benchmark(interp, 5, bench);

marks["3: hanoi"] = benchmark(interp, 1, hanoi);

marks["4: mandel"] = benchmark(interp, 1, mande);

console.log(marks);
