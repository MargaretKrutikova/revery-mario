open Revery.Math;

type collisionDirection = [ | `Left | `Right | `Up | `Down];

let createBoundingBox = (~x, ~y, ~height, ~width) => {
  let min = Reglm.Vec2.create(x, y);
  let max = Reglm.Vec2.create(x +. width, y +. height);
  BoundingBox2d.create(min, max);
};

let collides = (~entityBox: BoundingBox2d.t, ~contactBox: BoundingBox2d.t) => {
  let (entity_minX, entity_minY, entity_maxX, entity_maxY) =
    BoundingBox2d.getBounds(entityBox);
  let (contact_minX, contact_minY, contact_maxX, contact_maxY) =
    BoundingBox2d.getBounds(contactBox);

  let isInsideY = !(entity_maxY < contact_minY || entity_minY > contact_maxY);
  let isInsideX = !(entity_maxX < contact_minX || entity_minX > contact_maxX);
  print_endline(
    (entity_minX |> string_of_float) ++ "," ++ (entity_maxX |> string_of_float),
  );
  print_endline(
    (contact_minX |> string_of_float)
    ++ ","
    ++ (contact_maxX |> string_of_float),
  );

  if (entity_maxX > contact_minX && entity_minX < contact_minX && isInsideY) {
    Some(`Right);
  } else if (entity_minX < contact_maxX
             && entity_maxX > contact_maxX
             && isInsideY) {
    Some(`Left);
  } else {
    None// } else if (entity_maxY > entity_minY && isInsideX) {
        //   Some(`Up);
        ; //   Some(`Down);
 // else if (entity_minY < contact_maxY && isInsideX) {
        // }
  };
};
