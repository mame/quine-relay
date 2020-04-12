#!/usr/bin/env ruby
# encoding: utf-8
#
# Copyright (c) 2013 Michael Dvorkin
#
# Ante is an esoteric programming language where all you've got is
# a deck of cards.
#
# 95% of this code was developed on the way back from RubyConf 2013
# during 5-hour flight from Miami to San Francisco.
# 
### require "awesome_print"

class Array
  def rank; self[0] end
  def suit; self[1] end
  def rank=(value); self[0] = value end
  def suit=(value); self[1] = value end
end

class Ante
  def initialize()
    @♦, @♥, @♠, @♣ = 0, 0, 0, 0
    @line, @pc = 0, 0
    @code, @labels = [], {}
  end

  def run(source)
    parse(source)
    ### ap @code; ap @labels

    while card = @code[@pc]
      @pc += 1
      case card.rank
      when nil then newline(card)
      when "K" then jump(card)
      when "Q" then next
      when "J" then dump(card, :char)
      when 10  then dump(card)
      else          assign(card)
      end
    end
  end

  def parse(source)
    lines = source.split("\n").map { |line| line.sub(/#.*$/, "").strip }
    ### ap lines

    # Turn source file into array of cards. Each card is 2-item
    # array of rank and suit.
    lines.each_with_index do |line, i|
      @code += [[ nil, i + 1 ]] # <-- Line number cards have nil rank.
      @code += line.scan(/(10|[2-9JQKA])([♦♥♠♣])/)
    end

    # A pass to convert ranks to Fixnum and extract labels.
    pc = 0
    while card = @code[pc]
      pc += 1
      if card.rank =~ /\d/
        card.rank = card.rank.to_i
      elsif card.rank == "Q"
        queen = card.suit
        while @code[pc] && @code[pc].rank == "Q" && @code[pc].suit == card.suit
          queen += card.suit
          pc += 1
        end
        @labels[queen] = pc
      end
    end
  end

  def newline(card)
    # puts "newline #{card}"
    @line = card.suit
  end

  def assign(card)
    # puts "assign #{card.inspect}"
    operands = remaining(card)
    expression(operands)
  end

  def jump(card)
    # puts "jump #{card.inspect}, pc: #{@pc.inspect}, #{@labels.inspect}"
    suit = card.suit
    while @code[@pc] && @code[@pc].rank == "K" && @code[@pc].suit == card.suit
      suit += card.suit
      @pc += 1
    end

    if instance_variable_get("@#{suit[0]}") != 0
      # puts "jumping to " << "Q#{suit[0]}" * suit.size
      if @labels[suit]
        @pc = @labels[suit]
      else
        exception("can't find " << "Q#{suit[0]}" * suit.size << " to go to")
      end
    end
  end

  def dump(card, char = nil)
    # puts "dump #{card.inspect} => "
    value = instance_variable_get("@#{card.suit}")
    if char
      if value.between?(0, 255)
        print value.chr
      else
        exception("character code #{value} is out of 0..255 range")
      end
    else
      print value
    end
  end

  # Fetch the rest of the assignment expression.
  def remaining(card)
    operands = [ card ]
    while card = @code[@pc]
      break if card.rank.nil? || card.rank.to_s =~ /[KQJ]/
      operands += [ card ]
      @pc += 1
    end
    ### ap "remaining: #{operands.inspect}"
    operands
  end

  def expression(operands)
    initial, target = operands.shift
    initial = instance_variable_get("@#{target}") if initial == "A"
    operands.each do |rank, suit|
      # puts "rank: #{rank.inspect}, suit: #{suit.inspect}"
      rank = instance_variable_get("@#{suit}") if rank == "A"
      case suit
      when "♦" then initial += rank
      when "♥" then initial *= rank
      when "♠" then initial -= rank
      when "♣" then
        if rank != 0
          initial /= rank
        else
          exception("division by zero")
        end
      end
    end
    instance_variable_set("@#{target}", initial)
    # dump_registers
  end

  def exception(message)
    abort("Ante exception: #{message} on line #{@line} (pc:#{@pc})")
  end

  def dump_registers
    instance_variables.each do |i|
      puts "  #{i}: " + instance_variable_get("#{i}").to_s if i.size == 2
    end
  end
end

if ARGV[0]
  Ante.new.run(IO.read(ARGV[0], encoding: "UTF-8"))
else
  puts "usage: ante filename.ante"
end
