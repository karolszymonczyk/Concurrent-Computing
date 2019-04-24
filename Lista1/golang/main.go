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
}

type readTask struct {
	key  int
	task chan task
	resp chan string
}

type writeTask struct {
	val  task
	resp chan string
}

type printTasks struct {
	resp chan []task
}

type readItem struct {
	key     int
	product chan int
	resp    chan string
}

type writeItem struct {
	val  int
	resp chan string
}

type printStore struct {
	resp chan []int
}

var operands = [3]string{"+", "-", "*"}

var mode int

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

	for c := 0; c < config.CHAIRMANS_AMOUNT; c++ {
		go chairman(writesT)
	}

	time.Sleep(5*time.Millisecond)

	for w := 0; w < config.EMPLOYEES_AMOUNT; w++ {
		go employee(readsT, writeS)
	}

	for cl := 0; cl < config.CLIENTS_AMOUNT-1; cl++ {
		go client(readsS)
	}

	client(readsS)

	//time.Sleep(time.Second * 10)
}

func menu(printT chan *printTasks, printS chan *printStore) {
	var option int

	for {
		fmt.Println()
		fmt.Println("Choose option : ")
		fmt.Println("1. show store")
		fmt.Println("2. show tasks")
		fmt.Print(">> ")
		_, err := fmt.Scan(&option)
		fmt.Println()

		if err != nil {
			fmt.Println("Wrong input type")
			continue
		} else if option != 1 && option != 2 {
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
			if mode == 3 {
				fmt.Println("Chairman just added new task!")
			}
			write.resp <- "Chairman just added new task!"

		case read := <-maybeReadTask(len(state) > 0, readsT):
			read.task <- state[read.key]
			state = append(state[:0], state[0+1:]...)
			if mode == 3 {
				fmt.Println("Employee just took new task!")
			}
			read.resp <- "Employee just took new task!"

		case printList := <-printT:
			printList.resp <- state
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
			if mode == 3 {
				fmt.Println("Employee just finished the task")
			}
			write.resp <- "Employee just finished the task"

		case read := <-maybeReadStore(len(state) > 0, readsS):
			read.product <- state[read.key]
			state = append(state[:0], state[0+1:]...)
			if mode == 3 {
				fmt.Println("Client just bought product ")
			}
			read.resp <- "Client just bought product "

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

func chairman(writesT chan *writeTask) {
	for { //endless loop
		task := randomTask()
		write := &writeTask{
			val:  task,
			resp: make(chan string)}
		writesT <- write
		resp := <-write.resp
		if mode == 1 { //-----------------------------------------------
			fmt.Println(resp, task)
		}
		r := rand.Intn(config.CHAIRMAN_RANGE) //wczytany range z pliku config
		time.Sleep(time.Duration(r) * time.Millisecond)
	}
}

func randomTask() task {
	firstArg := rand.Intn(100)
	secArg := rand.Intn(100)
	operand := operands[rand.Intn(len(operands))]
	return task{firstArg, secArg, operand}
}

func employee(readsT chan *readTask, writesS chan *writeItem) { //ma ustalony stały czas
	for {
		read := &readTask{ //receiving tasks
			key:  0,
			task: make(chan task),
			resp: make(chan string)}
		readsT <- read
		task := <-read.task
		resp := <-read.resp
		if mode == 1 { //-----------------------------------------------
			fmt.Println(resp, task)
		}
		result := solveTask(task)

		write := &writeItem{
			val: result,
			resp: make(chan string)}
		writesS <- write
		respS := <-write.resp
		if mode == 1 { //-----------------------------------------------
			fmt.Println(respS, result)
		}
		time.Sleep(config.EMPLOYEE_DELAY * time.Millisecond) //delay from config file
	}
}

func solveTask(t task) int {

	var result int

	switch t.operator {
	case "+":
		result = t.firstArg + t.secArg
	case "-":
		result = t.firstArg - t.secArg
	case "*":
		result = t.firstArg * t.secArg
	}

	return result
}

func client(readsS chan *readItem) {
	for{
		read := &readItem{
			key: 0,
			product: make(chan int),
			resp: make(chan string)}
		readsS <- read
		product := <-read.product
		resp := <-read.resp
		if mode == 1 { //-----------------------------------------------
			fmt.Println(resp, product)
		}
		time.Sleep(config.CLIENT_DELAY * time.Millisecond)
	}
}
