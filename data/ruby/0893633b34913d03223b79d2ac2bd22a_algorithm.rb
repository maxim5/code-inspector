class Algorithm < ActiveRecord::Base
  attr_accessible :question, :trait
  
  PERSONALITY_TYPE={
	  1 => "Realistic",
	  2 => "Investigative",
	  3 => "Artistic",
	  4 => "Social",
	  5 => "Enterprising",
	  6 => "Conventional"
	}
end
