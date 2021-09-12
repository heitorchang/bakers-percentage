BAKERS_RECIPES = {
    "banana_bread": {
        "procedure": [
            "mix dry ingredients (except nuts) with a fork",
            "sift dry ingredients (except nuts)",
            "mix liquids",
            "add liquids to dry ingredients",
            "mix halfway (lightly)",
            "add nuts",
            "mix until all flour is moistened (do not overmix)",
            "bake at 190 deg. C for 25-30 minutes",
        ],
        "ingredients": [
            ("flour", 100),
            ("sugar", 40),
            ("baking_powder", 5),
        ]
    }
}


def recipes():
    return sorted(BAKERS_RECIPES.keys())


def ingredients(recipe_name):
    return BAKERS_RECIPES[recipe_name]["ingredients"]


def match_ingredient(recipe_name, ingredient_name):
    for ingredient in BAKERS_RECIPES[recipe_name]["ingredients"]:
        if ingredient[0] == ingredient_name:
            return ingredient[1]
    raise ValueError(ingredient_name + " not found")


def scale_ingredient(recipe_name, ingredient_name, new_weight):
    return new_weight / match_ingredient(recipe_name, ingredient_name)


def rescale(recipe_name, ingredient_name, new_weight):
    all_ingredients = ingredients(recipe_name)
    scale_factor = scale_ingredient(recipe_name, ingredient_name, new_weight)
    total_weight = 0

    for ingredient in all_ingredients:
        print(ingredient[0], scale_factor * ingredient[1])
