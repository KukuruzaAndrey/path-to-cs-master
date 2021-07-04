# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

# Enhancements
# 1. In your game, the player can press the ’u’ key to make the piece that is falling rotate 180 degrees.
# (Note it is normal for this to make some pieces appear to move slightly.)

# 2. In your game, instead of the pieces being randomly (and uniformly) chosen from the 7 classic pieces,
# the pieces are randomly (and uniformly) chosen from 10 pieces. They are the classic 7 and these 3:
# ✷✷              ✷
# ✷✷✷  ✷✷✷✷✷  ✷✷
# The initial rotation for each piece is also chosen randomly.

# 3. In your game, the player can press the ’c’ key to cheat: If the score is less than 100, nothing happens.
# Else the player loses 100 points (cheating costs you) and the next piece that appears will be:
# ✷
# The piece after is again chosen randomly from the 10 above (unless, of course, the player hits ’c’ while
# the “cheat piece” is falling and still has a large enough score). Hitting ’c’ multiple times while a single
# piece is falling should behave no differently than hitting it once.

class MyPiece < Piece
  # change Piece to MyPiece and All_Pieces to All_My_Pieces
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  # for 3
  def self.cheat_piece (board)
    MyPiece.new([[[0, 0]]], board)
  end
  
  # for 2
  All_My_Pieces = All_Pieces + [
      rotations([[0, 0], [1, 0], [-1, 0], [-1, -1], [0, -1]]),
      [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]],
       [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
      rotations([[0, 0], [0, 1], [1, 0]])
    ]
end

class MyBoard < Board
  # change Piece to MyPiece
  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    # for 3
    @cheat = false
  end
  
  # change Piece to MyPiece
  def next_piece
    # for 3
    if @cheat
      @current_block = MyPiece.cheat_piece(self)
      @score -= 100
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end
  
  # for 1
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 1)
      @current_block.move(0, 0, 1)
    end
    draw
  end
  
  # for 2
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    locations.each_with_index{|block, index| 
      @grid[block[1]+displacement[1]][block[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
  # for 3
  def try_cheat
    if @score >= 100
      @cheat = true
    end
  end

end

class MyTetris < Tetris
  # change Board to MyBoard
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  # for 1 and for 3
  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.try_cheat})
  end

end 

