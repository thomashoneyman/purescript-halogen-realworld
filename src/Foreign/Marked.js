// This module demonstrates how to leverage untyped JavaScript code in PureScript. This should be
// done sparingly. For a longer description of why we're using the FFI at all, see the
// `Conduit.Foreign.Marked` module in this directory.
//
// Before we even begin, note that we have created a JavaScript file (`Marked.js`) in the same folder
// as a PureScript file of the same name (`Marked.purs`). This convention ensures the compiler knows
// where to find the source for our foreign imports.

// Let's get started!

// First, we import the relevant JS library.
import { marked } from "marked";

// Next, we'll perform any setup necessary before exporting functions. In our case, we'll turn off
// the 'pedantic' option to be more lenient with parsing, and we'll enable GitHub-flavored
// markdown.
marked.setOptions({
  pedantic: false,
  gfm: true,
});

// Finally, we'll export one function that to use from PureScript. It will have a single argument:
// a markdown string to parse to an HTML string.
export const markedImpl = (str) => marked(str);
