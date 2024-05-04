use agb::{
    display::object::{Graphics, Tag},
    include_aseprite,
};

const SPRITES: &Graphics = include_aseprite!(
    "gfx/ship.aseprite",
    "gfx/bullet.aseprite",
    "gfx/big-rocks.aseprite",
    "gfx/small-rocks.aseprite",
    "gfx/numbers.aseprite"
);

pub const SHIP: &Tag = SPRITES.tags().get("SHIP");
pub const SHIP_BOOST: &Tag = SPRITES.tags().get("SHIP_BOOST");
pub const BULLET: &Tag = SPRITES.tags().get("BULLET");
pub const BIG_ROCKS: &Tag = SPRITES.tags().get("BIG_ROCKS");
pub const SMALL_ROCKS: &Tag = SPRITES.tags().get("SMALL_ROCKS");
pub const NUMBERS: &Tag = SPRITES.tags().get("NUMBERS");
