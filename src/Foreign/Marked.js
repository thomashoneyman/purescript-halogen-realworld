var marked = require("marked.js");

marked.setOptions({
  pedantic: false,
  gfm: true
});

exports.markedImpl = function(str) {
  return marked(str);
};
