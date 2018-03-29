data:extend(
{
	{
		type = "technology",
		name = "coal-processing",
		icon = "__Treefarm-Mod__/graphics/icons/charcoal.png",
		effects = {
			{
				type = "unlock-recipe",
				recipe = "charcoal"
			},
			{
				type = "unlock-recipe",
				recipe = "coal"
			},
			{
				type = "unlock-recipe",
				recipe = "coke-coal"
			},
			{
				type = "unlock-recipe",
				recipe = "ash"
			},
			{
				type = "unlock-recipe",
				recipe = "cokery"
			}
		},
		prerequisites = {
			"advanced-material-processing"
		},
		unit = {
			count = 75,
			ingredients = {
				{"science-pack-1", 1},
				{"science-pack-2", 1}
			},
			time = 30
		}
	},

	{
		type = "technology",
		name = "fertilizer",
		icon = "__Treefarm-Mod__/graphics/icons/fertilizer.png",
		effects = {
			{
				type = "unlock-recipe",
				recipe = "stone-crusher"
			},
			{
				type = "unlock-recipe",
				recipe = "crushed-stone"
			},
			{
				type = "unlock-recipe",
				recipe = "liquid-air"
			},
			{
				type = "unlock-recipe",
				recipe = "liquid-nitrogen"
			},
			{
				type = "unlock-recipe",
				recipe = "fill-liquid-nitrogen-barrel"
			},
			{
				type = "unlock-recipe",
				recipe = "empty-liquid-nitrogen-barrel"
			},
			{
				type = "unlock-recipe",
				recipe = "phosphate"
			},
			{
				type = "unlock-recipe",
				recipe = "potassium"
			},
			{
				type = "unlock-recipe",
				recipe = "fertilizer1"
			},
			{
				type = "unlock-recipe",
				recipe = "fertilizer2"
			},
			{
				type = "unlock-recipe",
				recipe = "hydroculture"
			},
			{
				type = "unlock-recipe",
				recipe = "biomass"
			}
		},
		prerequisites = {
			"fluid-handling"
		},
		unit = {
			count = 100,
			ingredients = {
				{"science-pack-1", 1},
				{"science-pack-2", 1}
			},
			time = 30
		}
	},

	{
		type = "technology",
		name = "organic-plastic",
		icon = "__Treefarm-Mod__/graphics/icons/cellulose.png",
		effects = {
			{
				type = "unlock-recipe",
				recipe = "cellulose"
			},
			{
				type = "unlock-recipe",
				recipe = "treefarm-mod-platic"
			}
		},
		prerequisites = {
			"fertilizer"
		},
		unit = {
			count = 50,
			ingredients = {
				{"science-pack-1", 2},
				{"science-pack-2", 2},
				{"science-pack-3", 1}
			},
			time = 30
		}
	},

	{
		type = "technology",
		name = "medicine",
		icon = "__Treefarm-Mod__/graphics/icons/medicine.png",
		effects = {
			{
				type = "unlock-recipe",
				recipe = "nutrients"
			},
			{
				type = "unlock-recipe",
				recipe = "medicine"
			}
		},
		prerequisites = {
			"fertilizer"
		},
		unit = {
			count = 75,
			ingredients = {
				{"science-pack-1", 2},
				{"science-pack-2", 2},
				{"science-pack-3", 1}
			},
			time = 45
		}
	},
}
)