class Team
  def initialize(*players)
    @players = players
  end

  def player_numbers
    @players.map { |player| player.number }
  end
end

class Player
  attr_reader :number

  def initialize(name, number)
    @name = name
    @number = number
  end
end

guy1 = Player.new('Bill', 23)
guy2 = Player.new('jeff', 18)

team = Team.new(guy1, guy2)
puts team.player_numbers