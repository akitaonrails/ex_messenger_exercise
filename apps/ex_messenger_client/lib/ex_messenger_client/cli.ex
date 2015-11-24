defmodule ExMessengerClient.CLI do
  alias ExMessengerClient.ServerProcotol

  def input_loop([server, nick]) do
    IO.write "#{Node.self}> "
    line = IO.read(:line)
      |> String.rstrip
    handle_command(line, [server, nick])
    input_loop([server, nick])
  end

  def handle_command("/help", _args) do
    IO.puts """
    Available commands:
      /leave
      /join
      /users
      /pm <to nick> <message>
      or just type a message to send
    """
  end

  def handle_command("/leave", args) do
    ServerProcotol.disconnect(args)
    IO.puts "You have exited the chatroom, you can rejoin with /join or quit with /quit"
  end

  def handle_command("/quit", args) do
    ServerProcotol.disconnect(args)
    System.halt(0)
  end

  def handle_command("/join", args) do
    ServerProcotol.connect(args)
    IO.puts "Joined the chatroom"
  end

  def handle_command("/users", args) do
    ServerProcotol.list_users(args)
  end

  def handle_command("", _args), do: :ok

  def handle_command(nil, _args), do: :ok

  def handle_command(message, args) do
    if String.contains?(message, "/pm") do
      {to, message} = parse_private_recipient(message)
      ServerProcotol.private_message(args, to, message)
    else
      ServerProcotol.say(args, message)
    end
  end

  defp parse_private_recipient(message) do
    [to|message] = message
      |> String.slice(4..-1)
      |> String.split
    message = message
      |> List.foldl("", fn(x, acc) -> "#{acc} #{x}" end)
      |> String.lstrip
    {to, message}
  end
end