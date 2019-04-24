package main

//todo problem producent-konsument

import (
	"./config"
	"fmt"
	"math/rand"
	"time"
)

type task struct {
	firstArg int
	secArg   int
	operator string
	result   int
}

type readTask struct {
	id   int
	key  int
	task chan task
	resp chan bool
}

type writeTask struct {
	id   int
	val  task
	resp chan bool
}

type printTasks struct {
	resp chan []task
}

type readItem struct {
	id      int
	key     int
	product chan int
	resp    chan bool
}

type writeItem struct {
	id   int
	val  int
	resp chan bool
}

type printStore struct {
	resp chan []int
}

type calcTask struct {
	id   int
	val  task
	resp chan task
}

type info struct {
	id      int
	typeE   string
	counter int
}

var operands = [2]string{"+", "*"}
var types = [2]string{"patient", "inpatient"}

var mode int

var employees [config.EMPLOYEES_AMOUNT]info
var addMachines [config.ADD_MACHINES_AMOUNT]chan *calcTask
var multiMachines [config.MULTI_MACHINES_AMOUNT]chan *calcTask

func main() {

	readsT := make(chan *readTask)
	writesT := make(chan *writeTask)
	printT := make(chan *printTasks)

	readsS := make(chan *readItem)
	writeS := make(chan *writeItem)
	printS := make(chan *printStore)

	for {
		fmt.Println()
		fmt.Println("Choose mode : ")
		fmt.Println("1. spectator")
		fmt.Println("2. silent")
		fmt.Print(">> ")

		_, err := fmt.Scan(&mode)

		if err != nil {
			fmt.Println("Wrong input type")
		} else if mode == 1 || mode == 2 {
			break
		} else {
			fmt.Println("Wrong input. Please try again")
		}
	}
	fmt.Println()

	if mode == 2 {
		go menu(printT, printS)
	}

	rand.Seed(time.Now().UnixNano())

	go tasksServer(readsT, writesT, printT)
	go storeServer(readsS, writeS, printS)

	for m := 0; m < config.MULTI_MACHINES_AMOUNT; m++ {
		multiMachines[m] = make(chan *calcTask)
		go multiMachine(m, multiMachines[m])
	}

	for a := 0; a < config.ADD_MACHINES_AMOUNT; a++ {
		addMachines[a] = make(chan *calcTask)
		go addingMachine(a, addMachines[a])
	}

	for c := 0; c < config.CHAIRMANS_AMOUNT; c++ {
		go chairman(c, writesT)
	}

	time.Sleep(3 * time.Millisecond)

	for w := 0; w < config.EMPLOYEES_AMOUNT; w++ {
		go employee(w, readsT, writeS, multiMachines, addMachines)
	}

	var counter = 0
	for cl := 0; cl < config.CLIENTS_AMOUNT-1; cl++ {
		go client(cl, readsS)
		counter++
	}

	client(counter, readsS)

	//time.Sleep(time.Second * 10)
}

func menu(printT chan *printTasks, printS chan *printStore) {
	var option int

	for {
		fmt.Println()
		fmt.Println("Choose option : ")
		fmt.Println("1. show store")
		fmt.Println("2. show tasks")
		fmt.Println("3. show employees")
		fmt.Print(">> ")
		_, err := fmt.Scan(&option)
		fmt.Println()

		if err != nil {
			fmt.Println("Wrong input type")
			continue
		} else if option != 1 && option != 2 && option != 3 {
			fmt.Println("Wrong input. Please try again")
			continue
		}
		fmt.Print()
		switch option {
		case 1:
			printStore := &printStore{
				resp: make(chan []int)}
			printS <- printStore
			resp := <-printStore.resp
			fmt.Println(resp)
		case 2:
			printList := &printTasks{
				resp: make(chan []task)}
			printT <- printList
			resp := <-printList.resp
			fmt.Println(resp)
		case 3:
			for _, employee := range employees {
				fmt.Println("ID :", employee.id, "  TYPE :", employee.typeE, "  SCORE :", employee.counter)
			}
		}
	}
}

func tasksServer(readsT chan *readTask, writesT chan *writeTask, printT chan *printTasks) {
	capacity := config.TASKS_CAPACITY
	var state []task
	for {
		select {
		case write := <-maybeWriteTask(len(state) < capacity, writesT):
			state = append(state, write.val)
			if mode == 1 {
				fmt.Println("Chairman", write.id, "added task :", write.val)
			}
			write.resp <- true

		case read := <-maybeReadTask(len(state) > 0, readsT):
			task := state[read.key]
			read.task <- task
			state = append(state[:0], state[0+1:]...)
			if mode == 1 {
				fmt.Println("Employee", read.id, "took task :", task)
			}
			read.resp <- true

		case printList := <-printT:
			printList.resp <- state
		}
	}
}

func multiMachine(id int, writesT chan *calcTask) {
	var busy = false
	for {
		select {
		case write := <-maybeCalc(!busy, writesT):
			busy = true
			if mode == 1 {
				fmt.Println("Employee", write.id, "put task :", write.val, "into multiplication machine", id)
			}
			write.val.result = write.val.firstArg * write.val.secArg

			time.Sleep(config.MULTI_MACHINE_TIME * time.Millisecond)
			if mode == 1 {
				fmt.Println("Employee", write.id, "took task :", write.val, "from multiplication machine", id)
			}
			write.resp <- write.val
			busy = false
		}
	}
}

func addingMachine(id int, writesT chan *calcTask) {
	var busy = false
	for {
		select {
		case write := <-maybeCalc(!busy, writesT):
			busy = true
			if mode == 1 {
				fmt.Println("Employee", write.id, "put task :", write.val, "into adding machine", id)
			}
			write.val.result = write.val.firstArg + write.val.secArg

			time.Sleep(config.ADD_MACHINE_TIME * time.Millisecond)
			if mode == 1 {
				fmt.Println("Employee", write.id, "took task :", write.val, "from adding machine", id)
			}
			write.resp <- write.val
			busy = false
		}
	}
}

func storeServer(readsS chan *readItem, writeS chan *writeItem, printS chan *printStore) {
	capacity := config.STORE_CAPACITY
	var state []int
	for {
		select {
		case write := <-maybeWriteStore(len(state) < capacity, writeS):
			state = append(state, write.val)
			if mode == 1 {
				fmt.Println("Employee", write.id, "put product :", write.val, "into store")
			}
			write.resp <- true

		case read := <-maybeReadStore(len(state) > 0, readsS):
			product := state[read.key]
			read.product <- product
			state = append(state[:0], state[0+1:]...)
			if mode == 1 {
				fmt.Println("Client", read.id, "bought :", product)
			}
			read.resp <- true

		case printStore := <-printS:
			printStore.resp <- state
		}
	}
}

func maybeReadTask(b bool, c chan *readTask) chan *readTask {
	if !b {
		return nil
	}
	return c
}

func maybeWriteTask(b bool, c chan *writeTask) chan *writeTask {
	if !b {
		return nil
	}
	return c
}

func maybeReadStore(b bool, c chan *readItem) chan *readItem {
	if !b {
		return nil
	}
	return c
}

func maybeWriteStore(b bool, c chan *writeItem) chan *writeItem {
	if !b {
		return nil
	}
	return c
}

func maybeCalc(b bool, c chan *calcTask) chan *calcTask {
	if !b {
		return nil
	}
	return c
}

func chairman(id int, writesT chan *writeTask) {
	for {
		task := randomTask()
		write := &writeTask{
			id:   id,
			val:  task,
			resp: make(chan bool)}
		writesT <- write
		<-write.resp
		r := rand.Intn(config.CHAIRMAN_RANGE)
		time.Sleep(time.Duration(r) * time.Millisecond)
	}
}

func randomTask() task {
	firstArg := rand.Intn(100)
	secArg := rand.Intn(100)
	operand := operands[rand.Intn(len(operands))]
	return task{firstArg, secArg, operand, 0}
}

func employee(id int, readsT chan *readTask, writesS chan *writeItem, multiMachines [config.MULTI_MACHINES_AMOUNT]chan *calcTask,
	addMachines [config.ADD_MACHINES_AMOUNT]chan *calcTask) {
	var typeE = chooseType()
	var counter = 0
	employees[id] = info{id, typeE, counter}
	for {
		read := &readTask{
			id:   id,
			key:  0,
			task: make(chan task),
			resp: make(chan bool)}
		readsT <- read
		taskR := <-read.task
		<-read.resp

		var output task
		calc := &calcTask{
			id:   id,
			val:  taskR,
			resp: make(chan task)}

		var machine chan *calcTask

		if taskR.operator == "+" {
			machine = chooseMachine(addMachines)
		} else {
			machine = chooseMachine(multiMachines)
		}

		if typeE == "inpatient" {
			done := false
			for !done {
				select {
				case machine <- calc:
					output = <-calc.resp
					done = true
				case <-time.After(config.TIME_TO_WAIT):
					if taskR.operator == "+" {
						machine = chooseMachine(addMachines)
					} else {
						machine = chooseMachine(multiMachines)
					}
				}
			}
		} else {
			machine <- calc
			output = <-calc.resp
		}

		result := output.result
		counter++
		employees[id].counter = counter

		write := &writeItem{
			id:   id,
			val:  result,
			resp: make(chan bool)}
		writesS <- write
		<-write.resp
		time.Sleep(config.EMPLOYEE_DELAY * time.Millisecond)
	}
}

func chooseMachine(machines [config.ADD_MACHINES_AMOUNT]chan *calcTask) chan *calcTask {
	return machines[rand.Intn(len(machines))]
}

func chooseType() string {
	return types[rand.Intn(len(types))]
}

func client(id int, readsS chan *readItem) {
	for {
		read := &readItem{
			id:      id,
			key:     0,
			product: make(chan int),
			resp:    make(chan bool)}
		readsS <- read
		<-read.product
		<-read.resp
		time.Sleep(config.CLIENT_DELAY * time.Millisecond)
	}
}
