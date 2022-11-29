module AdventOfCode.Commands

open System.CommandLine
let makeRoot () = new RootCommand()

let invoke (args: string array) (rootCommand: RootCommand) =
    rootCommand.Invoke(args)

let makeCommand name description = new Command(name, description)

let addCommand command (root: RootCommand) =
    root.AddCommand(command)
    root

let makeArg name description =
    new Argument<'a>(name = name, description = description)
    
let makeOptionalArg name description (getDefault: unit -> 'a) =
    new Argument<'a>(name, getDefault, description)
    
let addArg arg (command: Command) =
    command.AddArgument(arg)
    command
    
let addNewArg name description command = addArg (makeArg name description) command
    
let addOptionalArg name description getDefault command = addArg (makeOptionalArg name description getDefault) command

let setHandler2 (handler: 'a -> 'b -> unit) arg1 arg2 (command: Command) =
    command.SetHandler(handler, arg1, arg2)
    command


