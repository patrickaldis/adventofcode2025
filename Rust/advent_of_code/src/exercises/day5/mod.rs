type IngredientID = u32;

struct IngredientRange {
    start_id: IngredientID,
    stop_id: IngredientID
}

fn is_fresh(ingredient_ranges:&Vec<IngredientRange>, id:&IngredientID) -> bool {
    ingredient_ranges.into_iter().any(|IngredientRange {start_id, stop_id}| start_id <= id && id <= stop_id)
}

struct IngredientDB {
    ingredient_ranges: Vec<IngredientRange>,
    ingredients: Vec<IngredientID>
}

fn count_fresh(db: IngredientDB) -> u32 {
    let IngredientDB {ingredient_ranges, ingredients} = db;

    let fresh_ingredients = ingredients.iter().filter(|ingredient| is_fresh(&ingredient_ranges, &ingredient));

    fresh_ingredients.count() as u32
}
