data:extend(
{
  {
    type = "technology",
    name = "inserter_optimization",
    icon = "__MOD_Inserter__/graphics/technology/inserter_optimization.png",
    prerequisites = {
      "logistics"
    },
    effects = {
      {
        type = "unlock-recipe",
        recipe = "inserter_half"
      },
      {
        type = "unlock-recipe",
        recipe = "inserter_long"
      },
      {
        type = "unlock-recipe",
        recipe = "inserter_long_half"
      }
    },
    unit = {
      count = 30,
      ingredients = {
        {"science-pack-1", 1}
      },
      time = 10
    }
  },



  {
    type = "technology",
    name = "inserter_optimization_adv",
    icon = "__MOD_Inserter__/graphics/technology/inserter_optimization_adv.png",
    prerequisites = {
      "logistics-2",
      "inserter_optimization"
    },
    effects = {
      {
        type = "unlock-recipe",
        recipe = "inserter_fast_half"
      },
      {
        type = "unlock-recipe",
        recipe = "inserter_veryfast"
      },
      {
        type = "unlock-recipe",
        recipe = "inserter_veryfast_half"
      }
    },
    unit = {
      count = 60,
      ingredients = {
        {"science-pack-1", 1},
        {"science-pack-2", 1}
      },
      time = 30
    }
  },







  {
    type = "technology",
    name = "inserter_optimization_filter",
    icon = "__MOD_Inserter__/graphics/technology/inserter_optimization_adv.png",
    prerequisites = {
      "inserter_optimization_adv"
    },
    effects = {
      {
        type = "unlock-recipe",
        recipe = "inserter_fast_half_filter"
      },
      {
        type = "unlock-recipe",
        recipe = "inserter_veryfast_filter"
      },
      {
        type = "unlock-recipe",
        recipe = "inserter_veryfast_half_filter"
      }
    },
    unit = {
      count = 100,
      ingredients = {
        {"science-pack-1", 1},
        {"science-pack-2", 1}
      },
      time = 30
    }
  }


}
)