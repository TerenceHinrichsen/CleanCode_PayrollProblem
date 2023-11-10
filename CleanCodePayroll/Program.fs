module Domain =

   type SalariedEmployee = {
      MonthlySalary: decimal
   }

   // Commission is calculated by multiplying the commission rate by the total of the sales receipts
   // for that employee
   type CommissionedEmployee = {
      BaseSalary: decimal
      CommissionRate: decimal
   }

   // Pay is calculated by multiplying the hourly rate by the number of hours worked
   // If the number of hours worked is greater than 40, then the excess is multiplied by 1.5 of the hourly rate
   type HourlyEmployee = {
      HourlyRate: decimal
   }

   type Disposition =
      | MailedHome of {| AddressLine1 : string; AddressLine2 : string; City : string; State : string; ZipCode : string |}
      | HeldAtPaymasterOffice of {| OfficeNumber : string |}
      | DirectDeposit of {| BankName : string; AccountNumber : string; RoutingNumber : string |}

   type EmployeeType =
      | SalariedEmployee of SalariedEmployee
      | CommissionedEmployee of CommissionedEmployee
      | HourlyEmployee of HourlyEmployee

   type Employee =
      {
         EmployeeId: int
         EmployeeType : EmployeeType
         PayCheckDeliveryMethod : Disposition
      }

   type PayrollValue = | Hours of int | SalesReceipts of decimal
   type PayrollData =
      {
         EmployeeId: int
         PayrollValue: PayrollValue
      }

   module Test =
      let randomDisposition () =
         let rnd = System.Random()
         match rnd.Next(3) with
         | 0 -> MailedHome {| AddressLine1 = "123 Street"; AddressLine2 = "Apt 4"; City = "Cape Town"; State = "WC"; ZipCode = "8001" |}
         | 1 -> HeldAtPaymasterOffice {| OfficeNumber = "101" |}
         | _ -> DirectDeposit {| BankName = "Bank of SA"; AccountNumber = "123456789"; RoutingNumber = "987654321" |}

   module Database =

      //employee database:
      let employeeList =
         [
            {
               EmployeeId = 1
               EmployeeType = SalariedEmployee { MonthlySalary = 50000m }
               PayCheckDeliveryMethod = Test.randomDisposition()
            }
            {
               EmployeeId = 2
               EmployeeType = CommissionedEmployee { BaseSalary = 25000m; CommissionRate = 0.10m }
               PayCheckDeliveryMethod = Test.randomDisposition()
            }
            {
               EmployeeId = 3
               EmployeeType = HourlyEmployee { HourlyRate = 150m }
               PayCheckDeliveryMethod = Test.randomDisposition()
            }
            {
               EmployeeId = 4
               EmployeeType = HourlyEmployee { HourlyRate = 120m }
               PayCheckDeliveryMethod = Test.randomDisposition()
            }
            {
               EmployeeId = 5
               EmployeeType = HourlyEmployee { HourlyRate = 180m }
               PayCheckDeliveryMethod = Test.randomDisposition()
            }
            {
               EmployeeId = 6
               EmployeeType = HourlyEmployee { HourlyRate = 110m }
               PayCheckDeliveryMethod = Test.randomDisposition()
            }
         ]
      // monthly payroll data
      let payrollData =
         [
            {
               EmployeeId = 3
               PayrollValue = PayrollValue.Hours 46
            }
            {
               EmployeeId = 2
               PayrollValue = PayrollValue.SalesReceipts 10000m
            }
            {
               EmployeeId = 4
               PayrollValue = PayrollValue.Hours 39
            }
            {
               EmployeeId = 6
               PayrollValue = PayrollValue.Hours 40
            }
            {
               EmployeeId = 5
               PayrollValue = PayrollValue.Hours 50
            }
         ]

   module Helpers =
      let isPayDate (date: System.DateOnly) = function
         | SalariedEmployee _ -> // is last business day of the month
            let lastDayOfMonth = System.DateTime.DaysInMonth(date.Year, date.Month)
            date.Day = lastDayOfMonth
         | CommissionedEmployee _ -> //every other Friday
            date.DayOfWeek = System.DayOfWeek.Friday && date.DayNumber % 2 = 0
         | HourlyEmployee _ -> //every Friday
            date.DayOfWeek = System.DayOfWeek.Friday

      let calculateCommission payrollValue commissionRate =
         match payrollValue with
         | Hours _ -> failwith "Cannot calculate commission on hours"
         | SalesReceipts sr -> sr * commissionRate

      let calculateWage payrollValue (hourlyRate: decimal) =
         match payrollValue with
            | Hours h ->
               let h = decimal h
               if h > 40M then
                  let overtime = h - 40M
                  (40m * hourlyRate) + (overtime * hourlyRate * 1.5m)
                  else
                     h * hourlyRate
            | SalesReceipts _ -> failwith "Cannot calculate wage on sales receipts"

      let calculatePay payrollData (employee: Employee) =
         match employee.EmployeeType with
         | SalariedEmployee { MonthlySalary = salary } -> salary
         | CommissionedEmployee { BaseSalary = baseSalary; CommissionRate = commissionRate } ->
            let payrollValue =
               payrollData
               |> List.tryFind (fun p -> p.EmployeeId = employee.EmployeeId)
               |> Option.defaultValue {   EmployeeId = employee.EmployeeId
                                          PayrollValue = PayrollValue.SalesReceipts 0m }
               |> fun p -> p.PayrollValue
            let commission = calculateCommission payrollValue commissionRate
            baseSalary + commission
         | HourlyEmployee { HourlyRate = hourlyRate } ->
            let payrollValue =
               payrollData
               |> List.tryFind (fun p -> p.EmployeeId = employee.EmployeeId)
               |> Option.defaultValue {   EmployeeId = employee.EmployeeId
                                          PayrollValue = PayrollValue.Hours 0 }
               |> fun p -> p.PayrollValue
            calculateWage payrollValue hourlyRate

      let sendPay (employee: Employee) pay =
         match employee.PayCheckDeliveryMethod with
         | MailedHome md ->
            printfn $"For employeeId: {employee.EmployeeId}"
            printfn $"Mailing {pay} to {md.AddressLine1}, {md.AddressLine2}, {md.City}, {md.State}, {md.ZipCode}"
         | HeldAtPaymasterOffice on ->
            printfn $"For employeeId: {employee.EmployeeId}"
            printfn $"Holding {pay} at paymaster office {on.OfficeNumber}"
         | DirectDeposit eft ->
            printfn $"For employeeId: {employee.EmployeeId}"
            printfn $"Direct depositing {pay} to {eft.BankName} account {eft.AccountNumber} routing {eft.RoutingNumber}"

   [<EntryPoint>]
   let payRun _argv =
      let stopWatch = System.Diagnostics.Stopwatch.StartNew()
      let payRunDate = System.DateOnly.FromDateTime(System.DateTime.Today)
      printfn $"Performing pay run for {payRunDate}..."
      Database.employeeList
      |> List.filter (fun e -> Helpers.isPayDate payRunDate e.EmployeeType)
      |> List.map (fun e ->
         e, Helpers.calculatePay Database.payrollData e)
      |> List.iter (fun emp -> Helpers.sendPay (fst emp) (snd emp))
      stopWatch.Stop()
      printfn $"*** Pay run completed in {stopWatch.ElapsedMilliseconds} ms***"

      0
