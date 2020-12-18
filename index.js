import { Elm } from "./src/Main.elm";

console.log(process.env.API_URL);
Elm.Main.init({
  node: document.getElementById("main"),
  flags: process.env.API_URL,
});
