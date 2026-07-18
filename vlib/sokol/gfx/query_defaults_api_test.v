module gfx

import os

fn test_query_defaults_public_api_signatures_compile() {
	source_path := os.join_path(os.temp_dir(), 'sokol_query_defaults_api_${os.getpid()}.v')
	output_path := os.join_path(os.temp_dir(), 'sokol_query_defaults_api_${os.getpid()}.c')
	defer {
		os.rm(source_path) or {}
		os.rm(output_path) or {}
	}
	os.write_file(source_path, 'import sokol.gfx

fn accept_buffer_desc(_ gfx.BufferDesc) {}
fn accept_image_desc(_ gfx.ImageDesc) {}
fn accept_sampler_desc(_ gfx.SamplerDesc) {}
fn accept_shader_desc(_ gfx.ShaderDesc) {}
fn accept_pipeline_desc(_ gfx.PipelineDesc) {}
fn accept_attachments_desc(_ gfx.AttachmentsDesc) {}

fn query_defaults_api_probe(buffer gfx.Buffer, image gfx.Image, shader gfx.Shader, pipeline gfx.Pipeline, buffer_desc gfx.BufferDesc, image_desc gfx.ImageDesc, sampler_desc gfx.SamplerDesc, shader_desc gfx.ShaderDesc, pipeline_desc gfx.PipelineDesc, attachments_desc gfx.AttachmentsDesc) {
	accept_buffer_desc(gfx.query_buffer_defaults(&buffer))
	accept_image_desc(gfx.query_image_defaults(&image))
	accept_shader_desc(gfx.query_shader_defaults(&shader))
	accept_pipeline_desc(gfx.query_pipeline_defaults(&pipeline))
	accept_buffer_desc(gfx.query_buffer_desc_defaults(&buffer_desc))
	accept_image_desc(gfx.query_image_desc_defaults(&image_desc))
	accept_sampler_desc(gfx.query_sampler_defaults(&sampler_desc))
	accept_shader_desc(gfx.query_shader_desc_defaults(&shader_desc))
	accept_pipeline_desc(gfx.query_pipeline_desc_defaults(&pipeline_desc))
	accept_attachments_desc(gfx.query_attachments_defaults(&attachments_desc))
}

fn main() {}
')!
	command := '${os.quoted_path(@VEXE)} -b c -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
	result := os.execute(command)
	assert result.exit_code == 0, '${command}\n${result.output}'
}
