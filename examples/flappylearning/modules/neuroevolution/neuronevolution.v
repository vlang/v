module neuroevolution

import rand
import math

fn random_clamped() f64 {
	return rand.f64() * 2 - 1
}

pub fn activation(a f64) f64 {
	ap := (-a) / 1
	return (1 / (1 + math.exp(ap)))
}

fn round(a int, b f64) int {
	return int(math.round(f64(a) * b))
}

struct Neuron {
mut:
	value   f64
	weights []f64
}

fn (mut n Neuron) populate(nb int) {
	for _ in 0 .. nb {
		n.weights << random_clamped()
	}
}

struct Layer {
	id      int
mut:
	neurons []Neuron
}

fn (mut l Layer) populate(nb_neurons int, nb_inputs int) {
	for _ in 0 .. nb_neurons {
		mut n := Neuron{}
		n.populate(nb_inputs)
		l.neurons << n
	}
}

struct Network {
mut:
	layers []Layer
}

fn (mut n Network) populate(network []int) {
	assert network.len >= 2
	input := network[0]
	hiddens := network.slice(1, network.len - 1)
	output := network[network.len - 1]
	mut index := 0
	mut previous_neurons := 0
	mut input_layer := Layer{
		id: index
	}
	input_layer.populate(input, previous_neurons)
	n.layers << input_layer
	previous_neurons = input
	index++
	for hidden in hiddens {
		mut hidden_layer := Layer{
			id: index
		}
		hidden_layer.populate(hidden, previous_neurons)
		previous_neurons = hidden
		n.layers << hidden_layer
		index++
	}
	mut output_layer := Layer{
		id: index
	}
	output_layer.populate(output, previous_neurons)
	n.layers << output_layer
}

fn (n Network) get_save() Save {
	mut save := Save{}
	for layer in n.layers {
		save.neurons << layer.neurons.len
		for neuron in layer.neurons {
			for weight in neuron.weights {
				save.weights << weight
			}
		}
	}
	return save
}

fn (mut n Network) set_save(save Save) {
	mut previous_neurons := 0
	mut index := 0
	mut index_weights := 0
	n.layers = []
	for save_neuron in save.neurons {
		mut layer := Layer{
			id: index
		}
		layer.populate(save_neuron, previous_neurons)
		for mut neuron in layer.neurons {
			for i in 0 .. neuron.weights.len {
				neuron.weights[i] = save.weights[index_weights]
				index_weights++
			}
		}
		previous_neurons = save_neuron
		index++
		n.layers << layer
	}
}

pub fn (mut n Network) compute(inputs []f64) []f64 {
	assert n.layers.len > 0
	assert inputs.len == n.layers[0].neurons.len
	for i, input in inputs {
		n.layers[0].neurons[i].value = input
	}
	mut prev_layer := n.layers[0]
	for i in 1 .. n.layers.len {
		for j, neuron in n.layers[i].neurons {
			mut sum := f64(0)
			for k, prev_layer_neuron in prev_layer.neurons {
				sum += prev_layer_neuron.value * neuron.weights[k]
			}
			n.layers[i].neurons[j].value = activation(sum)
		}
		prev_layer = n.layers[i]
	}
	mut outputs := []f64{}
	mut last_layer := n.layers[n.layers.len - 1]
	for neuron in last_layer.neurons {
		outputs << neuron.value
	}
	return outputs
}

struct Save {
mut:
	neurons []int
	weights []f64
}

fn (s Save) clone() Save {
	mut save := Save{}
	save.neurons << s.neurons
	save.weights << s.weights
	return save
}

struct Genome {
	score   int
	network Save
}

struct Generation {
mut:
	genomes []Genome
}

fn (mut g Generation) add_genome(genome Genome) {
	mut i := 0
	for gg in g.genomes {
		if genome.score > gg.score {
			break
		}
		i++
	}
	g.genomes.insert(i, genome)
}

fn (g1 Genome) breed(g2 Genome, nb_child int) []Save {
	mut datas := []Save{}
	for _ in 0 .. nb_child {
		mut data := g1.network.clone()
		for i, weight in g2.network.weights {
			if rand.f64() <= 0.5 {
				data.weights[i] = weight
			}
		}
		for i, _ in data.weights {
			if rand.f64() <= 0.1 {
				data.weights[i] += (rand.f64() * 2 - 1) * 0.5
			}
		}
		datas << data
	}
	return datas
}

fn (g Generation) next(population int) []Save {
	mut nexts := []Save{}
	if population == 0 {
		return nexts
	}
	keep := round(population, 0.2)
	for i in 0 .. keep {
		if nexts.len < population {
			nexts << g.genomes[i].network.clone()
		}
	}
	random := round(population, 0.2)
	for _ in 0 .. random {
		if nexts.len < population {
			mut n := g.genomes[0].network.clone()
			for k, _ in n.weights {
				n.weights[k] = random_clamped()
			}
			nexts << n
		}
	}
	mut max := 0
	out: for {
		for i in 0 .. max {
			mut childs := g.genomes[i].breed(g.genomes[max], 1)
			for c in childs {
				nexts << c
				if nexts.len >= population {
					break out
				}
			}
		}
		max++
		if max >= g.genomes.len - 1 {
			max = 0
		}
	}
	return nexts
}

pub struct Generations {
pub:
	population  int
	network     []int
mut:
	generations []Generation
}

fn (mut gs Generations) first() []Save {
	mut out := []Save{}
	for _ in 0 .. gs.population {
		mut nn := Network{}
		nn.populate(gs.network)
		out << nn.get_save()
	}
	gs.generations << Generation{}
	return out
}

fn (mut gs Generations) next() []Save {
	assert gs.generations.len > 0
	gen := gs.generations[gs.generations.len - 1].next(gs.population)
	gs.generations << Generation{}
	return gen
}

fn (mut gs Generations) add_genome(genome Genome) {
	assert gs.generations.len > 0
	gs.generations[gs.generations.len - 1].add_genome(genome)
}

fn (mut gs Generations) restart() {
	gs.generations = []
}

pub fn (mut gs Generations) generate() []Network {
	saves := if gs.generations.len == 0 { gs.first() } else { gs.next() }
	mut nns := []Network{}
	for save in saves {
		mut nn := Network{}
		nn.set_save(save)
		nns << nn
	}
	if gs.generations.len >= 2 {
		gs.generations.delete(0)
	}
	return nns
}

pub fn (mut gs Generations) network_score(network Network, score int) {
	gs.add_genome(Genome{
		score: score
		network: network.get_save()
	})
}
