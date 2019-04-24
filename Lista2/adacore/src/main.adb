with config;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
use Ada.Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   type TaskS is
   record
         FirstArg : Integer;
         SecArg : Integer;
         Operator : Character;
         Result : Integer;
         State : Boolean;
      end record;

   type StoreItem is
      record
         Key : Integer;
         State : Boolean;
      end record;

   type Info is
      record
         id : Integer;
         typeE : Unbounded_String;
         counter : Integer;
      end record;

   MODE : Integer;
   CHOICE : Integer;

   type Sign is array (0 .. 1) of Character;
   Signs : Sign := ('+', '*');

   type TypeE is array (0 .. 1) of Unbounded_String;
   TypesE : TypeE := (To_Unbounded_String("patient"), To_Unbounded_String("inpatient"));

   EmployeesInfo : array (0 .. config.EMPLOYEES_AMOUNT-1) of Info;

   type TaskList is array (0 .. config.TASKS_CAPACITY) of TaskS;
   type StoreList is array (0 ..  config.STORE_CAPACITY) of StoreItem;

   task type Chairman is
      entry start(ID : in Integer);
   end Chairman;

   task type Employee is
      entry start(ID : in Integer);
   end Employee;

   task type Client is
      entry start(ID : in Integer);
   end Client;

   task type AddingMachine is
      entry start(ID : in Integer);
      entry Insert (An_Item : in TaskS; Out_Item : out TaskS);
   end AddingMachine;

   task type MultiMachine is
      entry start(ID : in Integer);
      entry Insert (An_Item : in TaskS; Out_Item : out TaskS);
   end MultiMachine;

   task TasksServer is
      entry Insert (An_Item : in TaskS);
      entry Remove (An_Item : out TaskS);
      entry Print (List : out TaskList);
   end TasksServer;

   task StoreServer is
      entry Insert (An_Item : in StoreItem);
      entry Remove (An_Item : out StoreItem);
      entry Print (List : out StoreList);
   end StoreServer;


   procedure PrintTasks (T : in TaskS) is  --PRINTTASKS
   begin
      Put("(" & Integer'Image(T.FirstArg) & Integer'Image(T.SecArg) & " " &
            T.Operator & Integer'Image(T.Result) & " ) ");
   end PrintTasks;

   procedure PrintStore (S : in StoreItem) is
   begin
      if S.Key = 0 then
         return;
      end if;
      Put("(" & Integer'Image(S.Key) & " ) ");
   end PrintStore;


   task body TasksServer is    --TASKSSERVER
      Q_Size : constant := config.TASKS_CAPACITY;
      subtype Q_Range is Positive range 1 .. Q_Size;
      Length : Natural range 0 .. Q_Size := 0;
      Head, Tail : Q_Range := 1;
      Data : TaskList;
   begin
      loop
         select
            when Length < Q_Size =>
               accept Insert (An_Item : in TaskS) do
                  Data(Tail) := An_Item;
               end Insert;
               Tail := Tail mod Q_Size + 1;
               Length := Length + 1;
         or
            when Length > 0 =>
               accept Remove (An_Item : out TaskS) do
                  Data(Head).State := False;
                  An_Item := Data(Head);
               end Remove;
               Head := Head mod Q_Size + 1;
               Length := Length - 1;
         or
            accept Print (List : out TaskList) do
               List := Data;
            end Print;
         end select;
      end loop;
   end TasksServer;


   task body MultiMachine is --MULTIMACHINE
      tempTask : TaskS;
      eID : Integer;
      Busy : Boolean := False;
      Result : Integer;
   begin
       accept start(ID : in Integer) do
         eID := ID;
       end start;
       loop
         select
            when Busy = False =>
               accept Insert (An_Item : in TaskS; Out_Item : out TaskS) do
                  Busy := True;

                  delay Duration(config.MULTI_MACHINE_TIME);

                  Result := An_Item.FirstArg * An_Item.SecArg;
                  tempTask := (An_Item.FirstArg, An_Item.SecArg, An_Item.Operator, Result, An_Item.State);
                  Out_Item := tempTask;
                  Busy := False;
               end Insert;
         end select;
      end loop;
   end MultiMachine;


   task body AddingMachine is --ADDINGMACHINE
      tempTask : TaskS;
      eID : Integer;
      Busy : Boolean := False;
      Result : Integer;
   begin
       accept start(ID : in Integer) do
         eID := ID;
       end start;
       loop
         select
            when Busy = False =>
               accept Insert (An_Item : in TaskS; Out_Item : out TaskS) do
                  Busy := True;

                  delay Duration(config.ADD_MACHINE_TIME);

                  Result := An_Item.FirstArg + An_Item.SecArg;
                  tempTask := (An_Item.FirstArg, An_Item.SecArg, An_Item.Operator, Result, An_Item.State);
                  Out_Item := tempTask;
                  Busy := False;
               end Insert;
         end select;
      end loop;
   end AddingMachine;


   task body StoreServer is   --STORESERVER
      Q_Size : constant := config.STORE_CAPACITY;
      subtype Q_Range is Positive range 1 .. Q_Size;
      Length : Natural range 0 .. Q_Size := 0;
      Head, Tail : Q_Range := 1;
      Data : StoreList;
   begin
      loop
         select
            when Length < Q_Size =>
               accept Insert (An_Item : in StoreItem) do
                  Data(Tail) := An_Item;
               end Insert;
               Tail := Tail mod Q_Size + 1;
               Length := Length + 1;
         or
            when Length > 0 =>
               accept Remove (An_Item : out StoreItem) do
                  Data(Head).State := False;
                  An_Item := Data(Head);
               end Remove;
               Head := Head mod Q_Size + 1;
               Length := Length - 1;
         or
            accept Print (List : out StoreList) do
               List := Data;
            end Print;
         end select;
      end loop;
   end StoreServer;


   function randomTask return TaskS is  --RANDOMTASK
      subtype Random_Range1 is Integer range 1 .. 100;
      subtype Random_Range2 is Integer range 0 .. 1;

      package R1 is new Ada.Numerics.Discrete_Random (Random_Range1);
      package R2 is new Ada.Numerics.Discrete_Random (Random_Range2);

      tempTask : TaskS;
      G1 : R1.Generator;
      G2 : R2.Generator;
      firstArg : Integer;
      secArg : Integer;
      Operation : Character;
   begin
      R1.Reset (G1);
      R2.Reset (G2);
      firstArg := R1.Random (G1);
      secArg := R1.Random (G1);
      Operation := Signs(R2.Random (G2));

      tempTask := (firstArg, secArg, Operation, 0, True);

      return tempTask;
   end randomTask;


   function chooseType return Unbounded_String is  --CHOOSETYPE
      subtype Random_Range is Integer range 0 .. 1;
      package R is new Ada.Numerics.Discrete_Random (Random_Range);

      G : R.Generator;
      TypeE : Unbounded_String;
     begin
      R.Reset (G);
      TypeE := TypesE(R.Random (G));

     return TypeE;
   end chooseType;


   function chooseMultiMachine return Integer is  --CHOOSETYPE
      subtype Random_Range is Integer range 1 .. config.MULTI_MACHINES_AMOUNT;
      package R is new Ada.Numerics.Discrete_Random (Random_Range);

      G : R.Generator;
     begin
      R.Reset (G);
     return R.Random (G);
   end chooseMultiMachine;


   function chooseAddingMachine return Integer is  --CHOOSETYPE
      subtype Random_Range is Integer range 1 .. config.ADD_MACHINES_AMOUNT;
      package R is new Ada.Numerics.Discrete_Random (Random_Range);

      G : R.Generator;
     begin
      R.Reset (G);
     return R.Random (G);
   end chooseAddingMachine;


   task body Chairman is   --CHAIRMAN
      task1 : TaskS;
      subtype Random_Range1 is Integer range 0 .. config.CHAIRMAN_RANGE-1;   --PLACE FOR FLOAT
      package R1 is new Ada.Numerics.Discrete_Random (Random_Range1);
      use Ada.Numerics.Float_Random;
      G1 : R1.Generator;
      G : Generator;
      WaitI : Integer;
      WaitF : Float;
      eID : Integer;
   begin
      accept start(ID : in Integer) do
         eID := ID;
      end start;
         Endless_Loop :
         loop

         task1 := randomTask;
         TasksServer.Insert(task1);
         if MODE = 1 then
            Put_Line("Chairman" & Integer'Image(eID) & " added task : (" & Integer'Image(task1.FirstArg) &
                    Integer'Image(task1.SecArg) & " " & task1.Operator & Integer'Image(task1.Result) & " )");
         end if;

         R1.Reset(G1);
         Reset(G);

         WaitI := R1.Random (G1);
         WaitF := Random (G);

         delay Duration(WaitI);
         delay Duration(WaitF);
         end loop Endless_Loop;

   end Chairman;


   type AddingMachines is array (Positive range 1 .. config.ADD_MACHINES_AMOUNT) of AddingMachine;
   C_AddingMachines : AddingMachines;

   type MultiMachines is array (Positive range 1 .. config.MULTI_MACHINES_AMOUNT) of MultiMachine;
   C_MultiMachines : MultiMachines;


   task body Employee is  --EMPLOYEE
      task1 : TaskS;
      taskM : TaskS;
      Machine : Integer;
      Done : Boolean;
      storeItem1 : StoreItem;
      result : Integer;
      eID : Integer;
      TypeE : Unbounded_String;
      Counter : Integer;
      InfoE : Info;
   begin
      accept start(ID : in Integer) do
         eID := ID;
         TypeE := chooseType;
         Counter := 0;
         InfoE := (eID, TypeE, Counter);
         EmployeesInfo(eID-1) := InfoE;
      end start;
      Endless_Loop :
      loop

         TasksServer.Remove(task1);
         if MODE = 1 then
            Put_Line("Employee" & Integer'Image(eID) & " took task : (" & Integer'Image(task1.FirstArg) &
                    Integer'Image(task1.SecArg) & " " & task1.Operator & Integer'Image(task1.Result) & " )");
         end if;

         if task1.Operator = '+' then
            Machine := chooseAddingMachine;
         else
            Machine := chooseMultiMachine;
         end if;

         if TypeE = "inpatient" then
            Done := false;
            if task1.Operator = '+' then
              while not Done loop
                 select
                     C_AddingMachines(Machine).Insert(task1, taskM);
                     Done := True;
                  or
                     delay Duration(config.TIME_TO_WAIT);
                     Machine := chooseAddingMachine;
                  end select;
               end loop;
            else
               while not Done loop
                 select
                     C_MultiMachines(Machine).Insert(task1, taskM);
                     Done := True;
                  or
                     delay Duration(config.TIME_TO_WAIT);
                     Machine := chooseMultiMachine;
                  end select;
               end loop;
            end if;
         else
            if task1.Operator = '+' then
               C_AddingMachines(Machine).Insert(task1, taskM);
            else
               C_MultiMachines(Machine).Insert(task1, taskM);
            end if;
         end if;

         result := taskM.Result;

         if MODE = 1 then
            Put_Line("Employee" & Integer'Image(eID) & " got task :" & " (" & Integer'Image(taskM.FirstArg) &
                    Integer'Image(taskM.SecArg) & " " & taskM.Operator & Integer'Image(taskM.Result) & " )" & " from machine :" & Integer'Image(Machine) );
         end if;

         Counter := Counter + 1;
         EmployeesInfo(eID-1).Counter := Counter;

         storeItem1 := (result, True);

         StoreServer.Insert(storeItem1);

         delay Duration(config.EMPLOYEE_DELAY);
      end loop Endless_Loop;
   end Employee;

   task body Client is --CLIENT
      product : StoreItem;
      eID : Integer;
   begin
      accept start(ID : in Integer) do
         eID := ID;
      end start;
      Endless_Loop :
      loop

         StoreServer.Remove(product);
         if MODE = 1 then
            Put_Line("Client" & Integer'Image(eID) & " bought product :" & Integer'Image(product.Key));
         end if;


         delay Duration(config.CLIENT_DELAY);
      end loop Endless_Loop;
   end Client;


   procedure Menu is         --MENU
      tl : TaskList;
      sl : StoreList;
   begin
      Endless_Loop :
      loop

         New_Line(1);
         Put_Line("Choose option : ");
         Put_Line("1. show store");
         Put_Line("2. show tasks");
         Put_Line("3. show employees info");
         Put(">> ");
         Get(CHOICE);
         New_Line(1);
         if CHOICE = 1 then
            StoreServer.Print(sl);
               for I in sl'Range loop
                  if sl(I).State then
                     PrintStore(sl(I));
                  end if;
               end loop;
               New_Line(1);
         elsif CHOICE = 2 then
            TasksServer.Print(tl);
               for I in tl'Range loop
                  if tl(I).State and (tl(I).Operator = '+'
                  or tl(I).Operator = '*') then
                     PrintTasks(tl(I));
                  end if;
               end loop;
            New_Line(1);
         elsif CHOICE = 3 then
            for I in EmployeesInfo'Range loop

               Put_Line("ID :" & Integer'Image(EmployeesInfo(I).id) & "  TYPE : " &
                          To_String(EmployeesInfo(I).typeE) & "  SCORE :" &
                          Integer'Image(EmployeesInfo(I).counter));
            end loop;
         end if;

      end loop Endless_Loop;
   end Menu;

   type Chairmans is array (Positive range 1 .. config.CHAIRMANS_AMOUNT) of Chairman;
   C_Chairmans : Chairmans;

   type Employees is array (Positive range 1 .. config.EMPLOYEES_AMOUNT) of Employee;
   C_Employees : Employees;

   type Clients is array (Positive range 1 .. config.CLIENTS_AMOUNT) of Client;
   C_Clients : Clients;


begin

   for num in C_AddingMachines'Range loop
      C_AddingMachines(num).start(num);
   end loop;

   for num in C_MultiMachines'Range loop
      C_MultiMachines(num).start(num);
   end loop;

   for num in C_Chairmans'Range loop
      C_Chairmans(num).start(num);
   end loop;

   for num in C_Employees'Range loop
      C_Employees(num).start(num);
   end loop;

   for num in C_Clients'Range loop
      C_Clients(num).start(num);
   end loop;

   Endless_Loop :
   loop

      New_Line(1);
      Put_Line("Choose mode : ");
      Put_Line("1. spectator");
      Put_Line("2. silent");
      Put(">> ");
      Get(MODE);
      if MODE = 1 then
         exit Endless_Loop;
      elsif MODE = 2 then
          Menu;
      end if;

   end loop Endless_Loop;

  end Main;


