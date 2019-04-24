with config;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
use Ada.Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   type TaskS is
   record
         FirstArg : Integer;
         SecArg : Integer;
         Operator : Character;
         State : Boolean;
      end record;

   type StoreItem is
      record
         Key : Integer;
         State : Boolean;
      end record;

   MODE : Integer;
   CHOICE : Integer;

   type Sign is array (0 .. 2) of Character;
   Signs : Sign := ('+', '-', '*');

   type TaskList is array (0 .. config.TASKS_CAPACITY) of TaskS;
   type StoreList is array (0 ..  config.STORE_CAPACITY) of StoreItem;

   task type Chairman;
   task type Employee;
   task type Client;

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
      Put("(");
      Put(Integer'Image(T.FirstArg));
      Put(Integer'Image(T.SecArg));
      Put(" ");
      Put(T.Operator);
      Put(" ) ");
   end PrintTasks;

   procedure PrintStore (S : in StoreItem) is
   begin
      Put("(");
      Put(Integer'Image(S.Key));
      Put(" ) ");
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
      subtype Random_Range2 is Integer range 0 .. 2;

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

      tempTask := (firstArg, secArg, Operation, True);

      return tempTask;
   end randomTask;


   function solveTask (T : in TaskS) return Integer is  --SOLVETASK
      result : Integer;
      sign : Character;
   begin
      sign := T.Operator;
      case sign is
      when '+' =>
         result := T.firstArg + T.secArg;
      when '-' =>
         result := T.firstArg - T.secArg;
      when '*' =>
         result := T.firstArg * T.secArg;
      when others =>
         result := 0;
      end case;

      return result;
   end solveTask;


   task body Chairman is   --CHAIRMAN
      task1 : TaskS;
      subtype Random_Range1 is Integer range 0 .. config.CHAIRMAN_RANGE-1;
      package R1 is new Ada.Numerics.Discrete_Random (Random_Range1);
      use Ada.Numerics.Float_Random;
      G1 : R1.Generator;
      G : Generator;
      WaitI : Integer;
      WaitF : Float;
   begin
         Endless_Loop :
         loop

         task1 := randomTask;
         TasksServer.Insert(task1);
         if MODE = 1 then
            Put_Line("Chairman just added new task!");
         end if;

         R1.Reset(G1);
         Reset(G);

         WaitI := R1.Random (G1);
         WaitF := Random (G);

         delay Duration(WaitI);
         delay Duration(WaitF);
         end loop Endless_Loop;
   end Chairman;


   task body Employee is  --EMPLOYEE
      task1 : TaskS;
      storeItem1 : StoreItem;
      result : Integer;
   begin
      Endless_Loop :
      loop

         TasksServer.Remove(task1);
         result := solveTask(task1);
         if MODE = 1 then
            Put_Line("Employee just resolved the task!");
         end if;

         storeItem1 := (result, True);

         StoreServer.Insert(storeItem1);

         delay Duration(config.EMPLOYEE_DELAY);
      end loop Endless_Loop;
   end Employee;

   task body Client is --CLIENT
      product : StoreItem;
   begin
      Endless_Loop :
      loop

         StoreServer.Remove(product);
         if MODE = 1 then
            Put_Line("Client just bought a product!");
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
                  or tl(I).Operator = '-' or tl(I).Operator = '*') then
                     PrintTasks(tl(I));
                  end if;
               end loop;
               New_Line(1);
         end if;

      end loop Endless_Loop;
   end Menu;

   Chairmans : array (0 .. config.CHAIRMANS_AMOUNT) of Chairman; -- CHAIRMANS
   Employees : array (0 .. config.EMPLOYEES_AMOUNT) of Employee; -- EMPLOYEES
   Clients : array (0 .. config.CLIENTS_AMOUNT) of Client;       -- CLIENTS

begin

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


