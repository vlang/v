import time
import sync

type Callback = fn (id string)

fn producer(producer_name string, mut arr []Callback, mut mtx sync.Mutex) {
	for i in 1 .. 5 {
		mtx.lock()
		arr << fn [producer_name, i] (consumer_name string) {
			println('task ${i} created by producer ${producer_name}: consumed by ${consumer_name}')
			time.sleep(500 * time.millisecond)
		}
		println('Produced: ${i}')
		time.sleep(50 * time.millisecond)
		mtx.unlock()
	}
}

fn consumer(consumer_name string, mut arr []Callback, mut mtx sync.Mutex) {
	for {
		mtx.lock()
		if arr.len > 0 {
			item := arr[0]
			arr.delete(0)

			mtx.unlock()
			item(consumer_name) // run after unlocking to allow other threads to consume
			continue
		} else {
			println('- No items to consume')
			mtx.unlock()

			// time.sleep(500 * time.millisecond)
			// continue // uncomment to run forever

			break // uncomment to stop after consuming all items
		}
	}
}

fn heavy_processing(queue_id string) {
	println('One more: ${queue_id}')
	time.sleep(500 * time.millisecond)
}

fn main() {
	mut mtx := sync.new_mutex()
	mut arr := []Callback{}

	producer_threads := [
		spawn producer('Paula', mut &arr, mut mtx),
		spawn producer('Adriano', mut &arr, mut mtx),
		spawn producer('Kaka', mut &arr, mut mtx),
		spawn producer('Hitalo', mut &arr, mut mtx),
		spawn producer('Jonh', mut &arr, mut mtx),
	]

	mut consumer_threads := [
		spawn consumer('consumer number 0', mut &arr, mut mtx),
	]

	// spawn 16 consumers
	for i in 1 .. 16 {
		consumer_threads << spawn consumer('consumer number ${i}', mut &arr, mut mtx)
	}

	mtx.lock()
	arr << heavy_processing
	mtx.unlock()

	for t in producer_threads {
		t.wait()
	}
	for t in consumer_threads {
		t.wait()
	}
}
