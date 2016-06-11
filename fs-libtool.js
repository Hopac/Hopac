"use strict";

(function() {
  function hop(op, k) {setTimeout(function() {op(k)}, 0)}
  function ignore() {}
  function queue(op) {hop(op, ignore)}

  function par() {
    var ops = Array.prototype.slice.call(arguments)
    return function(k) {
      var n = ops.length
      if (!n) {
        k()
      } else {
        function co() {
          if (!--n)
            k()
        }
        for (var i=0; i<ops.length; ++i)
          ops[i](co)
      }
    }
  }

  function seq() {
    var ops = Array.prototype.slice.call(arguments)
    return function(k) {
      var i = 0
      function lp() {
        if (i < ops.length)
          ops[i++](lp)
        else
          k()
      }
      lp()
    }
  }

  function loadScript(url) {
    return function(k) {
      var script = document.createElement("script")
      script.onload = k
      script.type = "text/javascript"
      script.src = url
      document.getElementsByTagName("head")[0].appendChild(script)
    }
  }

  var init = function(k) {
    $(document).ready(function() {
      $("code").each(function(i, block) {
        hljs.highlightBlock(block);
      })})
    k()
  }

  queue(seq(par(seq(loadScript("https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.4.0/highlight.min.js"),
                    loadScript("https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.4.0/languages/fsharp.min.js")),
                loadScript("https://cdnjs.cloudflare.com/ajax/libs/jquery/2.2.4/jquery.min.js")),
            init))
})()
