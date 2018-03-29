class Algorithm < ActiveRecord::Base
  has_many :plans
  has_many :algorithm_comparison_joins
  has_many :comparison_types, :through => :algorithm_comparison_joins
  
end
