# Algorithm G, pp.62 TAOCPvII

class GapTest
  attr_accessor :range, :alpha, :beta, :j, :s, :r, :count

  def initialize(range, alpha=0, beta=1)
    self.range = range
    self.alpha = alpha
    self.beta = beta
    self.j = -1
    self.s = 0
    self.count = Hash.new
  end

  def set_r
    self.r = 0
  end

  def test_value
    self.j += 1
    if self.range[self.j] >= self.alpha || self.range[self.j < beta
      self.record_gap_length
    else
      self.inc_r
    end
  end

  def inc_r
    self.r += 1
    self.test_value
  end

  def record_gap_length
    if r >= t
      self.count[t] += 1
    else
      self.count[r] += 1
    end
  end

  def n_gaps_found?
    self.s += 1
    if s < n
      self.set_r(r) 
    else
      self.chi_squared_test
    end
  end

  def chi_squared_test
    
  end
end

