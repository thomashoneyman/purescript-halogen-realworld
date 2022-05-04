// Halogen does not supply a way to set an HTML string in the DOM. This snippet will be foreign-
// imported into the `RawHTML.purs` module so we can support this behavior. For a much fuller
// example of how to properly use the PureScript FFI with JavaScript, please see the `Marked.js`
// file, which supports the `Conduit.Foreign.Marked` module.
export function unsafeSetInnerHTML(element) {
  return function (html) {
    return function () {
      element.innerHTML = html;
    };
  };
}
