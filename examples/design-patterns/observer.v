module main

import rand

// Subject holds the state and manages subscribers.
struct Subject {
mut:
	state     int
	observers []Observer
}

// Observer declares the update method.
interface Observer {
	// Receive update from subject.
	update(subject &Subject)
}

// Attach an observer to the subject.
fn (mut subject Subject) attach(observer Observer) {
	subject.observers << observer
	println('Subject: Attached an observer.')
}

// Detach an observer from the subject.
fn (mut subject Subject) detach(observer Observer) {
	mut index := -1
	for i, obs in subject.observers {
		if obs == observer {
			index = i
			break
		}
	}

	if index != -1 {
		subject.observers = []
		subject.observers << subject.observers[..index]
		subject.observers << subject.observers[index + 1..]
		println('Subject: Detached an observer.')
	} else {
		println('Subject: Nonexistent observer.')
	}
}

// Notify all observers about an event.
fn (subject Subject) notify() {
	println('Subject: Notifying observers...')
	for observer in subject.observers {
		observer.update(&subject)
	}
}

// ConcreteSubject implements the Subject interface.
struct ConcreteSubject {
	Subject
}

// The subscription logic is only a fraction of what a Subject can do.
fn (mut subject ConcreteSubject) some_business_logic() {
	println("\nSubject: I'm doing something important.")
	subject.state = rand.intn(11) or { 0 }
	println('Subject: My state has just changed to: ${subject.state}')
	subject.notify()
}

// ConcreteObserverA implements the Observer interface.
struct ConcreteObserverA {}

// Receive update from subject.
fn (observer ConcreteObserverA) update(subject &Subject) {
	if subject.state < 3 {
		println('ConcreteObserverA: Reacted to the event.')
	}
}

// ConcreteObserverB implements the Observer interface.
struct ConcreteObserverB {}

// Receive update from subject.
fn (observer ConcreteObserverB) update(subject &Subject) {
	if subject.state == 0 || subject.state >= 2 {
		println('ConcreteObserverB: Reacted to the event.')
	}
}

fn main() {
	mut subject := ConcreteSubject{}

	observer1 := ConcreteObserverA{}
	subject.attach(&observer1)

	observer2 := ConcreteObserverB{}
	subject.attach(&observer2)

	subject.some_business_logic()
	subject.some_business_logic()

	subject.detach(&observer2)

	subject.some_business_logic()
}
