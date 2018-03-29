DATA = {
  "A"   => "something",
  "B"   => "nothing"
}

Given /^I am in group "([^\"]*)"$/ do |group|
  @group = group
end

Then /^I should see "([^\"]*)"$/ do |text|
  DATA[@group].should == text
end

