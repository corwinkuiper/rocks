use agb::{
    display::object::{Graphics, Tag},
    include_aseprite,
};

static SPRITES: &Graphics = include_aseprite!(
    "gfx/ship.aseprite",
    "gfx/bullet.aseprite",
    "gfx/big-rocks.aseprite",
    "gfx/small-rocks.aseprite",
    "gfx/numbers.aseprite"
);

pub static SHIP: &Tag = SPRITES.tags().get("SHIP");
pub static SHIP_BOOST: &Tag = SPRITES.tags().get("SHIP_BOOST");
pub static BULLET: &Tag = SPRITES.tags().get("BULLET");
pub static BIG_ROCKS: &Tag = SPRITES.tags().get("BIG_ROCKS");
pub static SMALL_ROCKS: &Tag = SPRITES.tags().get("SMALL_ROCKS");
pub static NUMBERS: &Tag = SPRITES.tags().get("NUMBERS");
pub static SHIP_PARTS: &Tag = SPRITES.tags().get("SHIP_PARTS");
