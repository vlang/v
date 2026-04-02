// vtest build: !docker-ubuntu-musl // needs GL/gl.h
module sgl

fn test_next_draw_chunk_keeps_uncapped_draws_intact() {
	base_vertex, num_vertices := next_draw_chunk(10, 20, 0)
	assert base_vertex == 10
	assert num_vertices == 20
}

fn test_next_draw_chunk_caps_large_point_batches() {
	total_vertices := max_point_batch_vertices + 257
	first_base_vertex, first_num_vertices := next_draw_chunk(0, total_vertices, max_point_batch_vertices)
	assert first_base_vertex == 0
	assert first_num_vertices == max_point_batch_vertices
	second_base_vertex, second_num_vertices := next_draw_chunk(first_base_vertex +
		first_num_vertices, total_vertices - first_num_vertices, max_point_batch_vertices)
	assert second_base_vertex == max_point_batch_vertices
	assert second_num_vertices == 257
}
