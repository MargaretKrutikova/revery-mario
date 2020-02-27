module Floor = {
  let image = "floor-light.png";
  let height = 30;
  let width = 30;
};
// 271 / 411
// w / h
// 80 /
module Pipe = {
  let image = "pipe2.png";
  let width = 80;
  let height = 167; // (569.0 /. 271.0 |> int_of_float) * width;
};

module Mario = {
  module Moving = {
    let height = 55;
    let width = 40;
    let image1 = "mario-1.png";
    let image2 = "mario-2.png";
    let image3 = "mario-3.png";
  };

  module Jump = {
    let height = 46;
    let width = 47;
    let image = "mario-jump.png";
  };

  module Idle = {
    let height = 50;
    let width = 40;
    let image = "mario-idle.png";
  };
};
