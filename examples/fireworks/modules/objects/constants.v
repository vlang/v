module objects

pub struct UIParams {
pub mut:
	dpi_scale       f32    = 1.0
	width           int    = 800
	height          int    = 800
	gravity         Vector = Vector{0, -0.03}
	age_rate        int    = 1
	offspring_count int    = 100
	rocket_radius   int    = 5
	particle_radius f32    = 2.5
	drag            f32    = 0.98
}

const params = &UIParams{}

pub fn get_params() &UIParams {
	return objects.params
}
