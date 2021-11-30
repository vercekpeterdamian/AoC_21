open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type instruction = Acc of int | Jmp of int | Nop of int

  type state = {
    instruction_pointer : int;
    accumulator : int;
    history : int list Array.t;
    step : int;
  }

  type config = { initial_instructions : instruction Array.t; state : state }

  type step_result = Running of config | Halt of config

  type loop_result = Loop of config | Finished of config

  module Machine = struct
    let initial_state history =
      { instruction_pointer = 0; accumulator = 0; step = 0; history }

    let parse_line line =
      let[@warning "-8"] [ ins; arg ] = String.split_on_char ' ' line in
      match ins with
      | "nop" -> Nop (int_of_string arg)
      | "jmp" -> Jmp (int_of_string arg)
      | "acc" -> Acc (int_of_string arg)
      | _ -> failwith "Parsing error"

    let clean config =
      {
        config with
        state =
          initial_state
            (Array.init (Array.length config.initial_instructions) (fun _ -> []));
      }

    let parse_data data =
      let instructions =
        List.lines data |> List.map parse_line |> Array.of_list
      in
      {
        initial_instructions = instructions;
        state =
          initial_state (Array.init (Array.length instructions) (fun _ -> []));
      }

    let step ({ state; initial_instructions } as config) =
      if state.instruction_pointer >= Array.length initial_instructions then
        Halt config
      else
        let ({ state; initial_instructions } as config) =
          match initial_instructions.(state.instruction_pointer) with
          | Nop _ ->
              {
                config with
                state =
                  {
                    state with
                    instruction_pointer = state.instruction_pointer + 1;
                  };
              }
          | Acc x ->
              {
                config with
                state =
                  {
                    state with
                    instruction_pointer = state.instruction_pointer + 1;
                    accumulator = state.accumulator + x;
                  };
              }
          | Jmp j ->
              {
                config with
                state =
                  {
                    state with
                    instruction_pointer = state.instruction_pointer + j;
                  };
              }
        in
        if state.instruction_pointer >= Array.length initial_instructions then
          Halt config
        else Running config

    let rec run_til_loop ({ state; _ } as config) =
      let ip = state.instruction_pointer in
      match state.history.(ip) with
      | [] -> (
          config.state.history.(ip) <- [ state.step ];
          match step config with
          | Halt c -> Finished c
          | Running ({ state; _ } as config) ->
              run_til_loop
                { config with state = { state with step = state.step + 1 } })
      | _ -> Loop config

    let change = function Nop x -> Jmp x | Jmp x -> Nop x | x -> x

    let to_str = function
      | Nop x -> "Nop: " ^ string_of_int x
      | Jmp x -> "Jmp: " ^ string_of_int x
      | Acc x -> "Acc: " ^ string_of_int x

    let _print_instructions config =
      print_endline
        (String.concat "; "
           (List.map to_str (Array.to_list config.initial_instructions)))

    let try_machine initial_config =
      let rec try' i config =
        let last = config.initial_instructions.(i) in
        match last with
        | Acc _ -> try' (i + 1) config
        | _ -> (
            config.initial_instructions.(i) <- change last;
            match run_til_loop config with
            | Finished c -> c
            | Loop _ ->
                config.initial_instructions.(i) <- last;
                try' (i + 1) (clean config))
      in
      try' 0 initial_config
  end

  let naloga1 data =
    let machine = Machine.parse_data data in
    let[@warning "-8"] (Loop final) = Machine.run_til_loop machine in
    final.state.accumulator |> string_of_int

  let naloga2 data _part1 =
    let machine = Machine.parse_data data in
    let final = Machine.try_machine machine in
    final.state.accumulator |> string_of_int
end
