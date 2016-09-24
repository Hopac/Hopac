namespace Hopac.Bench

module Async =
  let unit = async.Zero ()
  let inline result x = async.Return x

  module Infixes =
    let inline (>>=) xA x2yA = async.Bind (xA, x2yA)
    let inline (>>-) xA x2y = async.Bind (xA, x2y >> result)

  module Extensions =
    module Seq =
      let inline iterAsync x2uA xs = async.For (xs, x2uA)
