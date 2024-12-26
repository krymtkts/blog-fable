import { spawn } from "node:child_process";

const arg = process.argv[2];
console.log("Run App.fs.js with mode:", arg);

const run = () =>
  new Promise((resolve, reject) => {
    process.env.NODE_ENV = "production";
    const child = spawn("node", ["./src/App.fs.js", arg]);

    child.stdout.on("data", (data) => {
      process.stdout.write(data);
    });
    child.stderr.on("data", (data) => {
      process.stderr.write(data);
    });
    child.on("close", (code) => {
      if (code !== 0) {
        reject(new Error(`Child process exited with code ${code}`));
      } else {
        resolve();
      }
    });
  });

try {
  await run();
} catch (error) {
  console.error("Error running script:", error);
  exit(1);
}
