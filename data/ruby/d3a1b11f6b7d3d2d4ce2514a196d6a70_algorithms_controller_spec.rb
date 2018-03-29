require File.dirname(__FILE__) + '/../spec_helper'

describe AlgorithmsController, "#route_for" do

  it "should map { :controller => 'algorithms', :action => 'index' } to /algorithms" do
    route_for(:controller => "algorithms", :action => "index").should == "/algorithms"
  end
  
  it "should map { :controller => 'algorithms', :action => 'new' } to /algorithms/new" do
    route_for(:controller => "algorithms", :action => "new").should == "/algorithms/new"
  end
  
  it "should map { :controller => 'algorithms', :action => 'show', :id => 1 } to /algorithms/1" do
    route_for(:controller => "algorithms", :action => "show", :id => 1).should == "/algorithms/1"
  end
  
  it "should map { :controller => 'algorithms', :action => 'edit', :id => 1 } to /algorithms/1/edit" do
    route_for(:controller => "algorithms", :action => "edit", :id => 1).should == "/algorithms/1/edit"
  end
  
  it "should map { :controller => 'algorithms', :action => 'update', :id => 1} to /algorithms/1" do
    route_for(:controller => "algorithms", :action => "update", :id => 1).should == "/algorithms/1"
  end
  
  it "should map { :controller => 'algorithms', :action => 'destroy', :id => 1} to /algorithms/1" do
    route_for(:controller => "algorithms", :action => "destroy", :id => 1).should == "/algorithms/1"
  end
  
end

describe AlgorithmsController, "#params_from" do

  it "should generate params { :controller => 'algorithms', action => 'index' } from GET /algorithms" do
    params_from(:get, "/algorithms").should == {:controller => "algorithms", :action => "index"}
  end
  
  it "should generate params { :controller => 'algorithms', action => 'new' } from GET /algorithms/new" do
    params_from(:get, "/algorithms/new").should == {:controller => "algorithms", :action => "new"}
  end
  
  it "should generate params { :controller => 'algorithms', action => 'create' } from POST /algorithms" do
    params_from(:post, "/algorithms").should == {:controller => "algorithms", :action => "create"}
  end
  
  it "should generate params { :controller => 'algorithms', action => 'show', id => '1' } from GET /algorithms/1" do
    params_from(:get, "/algorithms/1").should == {:controller => "algorithms", :action => "show", :id => "1"}
  end
  
  it "should generate params { :controller => 'algorithms', action => 'edit', id => '1' } from GET /algorithms/1;edit" do
    params_from(:get, "/algorithms/1/edit").should == {:controller => "algorithms", :action => "edit", :id => "1"}
  end
  
  it "should generate params { :controller => 'algorithms', action => 'update', id => '1' } from PUT /algorithms/1" do
    params_from(:put, "/algorithms/1").should == {:controller => "algorithms", :action => "update", :id => "1"}
  end
  
  it "should generate params { :controller => 'algorithms', action => 'destroy', id => '1' } from DELETE /algorithms/1" do
    params_from(:delete, "/algorithms/1").should == {:controller => "algorithms", :action => "destroy", :id => "1"}
  end
  
end

describe AlgorithmsController, "handling GET /algorithms" do

  before do
    @algorithm = mock_model(Algorithm)
    Algorithm.stub!(:find).and_return([@algorithm])
  end
  
  def do_get
    get :index
  end
  
  it "should be successful" do
    do_get
    response.should be_success
  end

  it "should render index template" do
    do_get
    response.should render_template('index')
  end
  
  it "should find all algorithms" do
    Algorithm.should_receive(:find).with(:all).and_return([@algorithm])
    do_get
  end
  
  it "should assign the found algorithms for the view" do
    do_get
    assigns[:algorithms].should == [@algorithm]
  end
end

describe AlgorithmsController, "handling GET /algorithms.xml" do

  before do
    @algorithm = mock_model(Algorithm, :to_xml => "XML")
    Algorithm.stub!(:find).and_return(@algorithm)
  end
  
  def do_get
    @request.env["HTTP_ACCEPT"] = "application/xml"
    get :index
  end
  
  it "should be successful" do
    do_get
    response.should be_success
  end

  it "should find all algorithms" do
    Algorithm.should_receive(:find).with(:all).and_return([@algorithm])
    do_get
  end
  
  it "should render the found algorithms as xml" do
    @algorithm.should_receive(:to_xml).and_return("XML")
    do_get
    response.body.should == "XML"
  end
end

describe AlgorithmsController, "handling GET /algorithms/1" do

  before do
    @algorithm = mock_model(Algorithm)
    Algorithm.stub!(:find).and_return(@algorithm)
  end
  
  def do_get
    get :show, :id => "1"
  end

  it "should be successful" do
    do_get
    response.should be_success
  end
  
  it "should render show template" do
    do_get
    response.should render_template('show')
  end
  
  it "should find the algorithm requested" do
    Algorithm.should_receive(:find).with("1").and_return(@algorithm)
    do_get
  end
  
  it "should assign the found algorithm for the view" do
    do_get
    assigns[:algorithm].should equal(@algorithm)
  end
end

describe AlgorithmsController, "handling GET /algorithms/1.xml" do

  before do
    @algorithm = mock_model(Algorithm, :to_xml => "XML")
    Algorithm.stub!(:find).and_return(@algorithm)
  end
  
  def do_get
    @request.env["HTTP_ACCEPT"] = "application/xml"
    get :show, :id => "1"
  end

  it "should be successful" do
    do_get
    response.should be_success
  end
  
  it "should find the algorithm requested" do
    Algorithm.should_receive(:find).with("1").and_return(@algorithm)
    do_get
  end
  
  it "should render the found algorithm as xml" do
    @algorithm.should_receive(:to_xml).and_return("XML")
    do_get
    response.body.should == "XML"
  end
end

describe AlgorithmsController, "handling GET /algorithms/new" do

  before do
    @algorithm = mock_model(Algorithm)
    Algorithm.stub!(:new).and_return(@algorithm)
  end
  
  def do_get
    get :new
  end

  it "should be successful" do
    do_get
    response.should be_success
  end
  
  it "should render new template" do
    do_get
    response.should render_template('new')
  end
  
  it "should create an new algorithm" do
    Algorithm.should_receive(:new).and_return(@algorithm)
    do_get
  end
  
  it "should not save the new algorithm" do
    @algorithm.should_not_receive(:save)
    do_get
  end
  
  it "should assign the new algorithm for the view" do
    do_get
    assigns[:algorithm].should equal(@algorithm)
  end
end

describe AlgorithmsController, "handling GET /algorithms/1/edit" do

  before do
    @algorithm = mock_model(Algorithm)
    Algorithm.stub!(:find).and_return(@algorithm)
  end
  
  def do_get
    get :edit, :id => "1"
  end

  it "should be successful" do
    do_get
    response.should be_success
  end
  
  it "should render edit template" do
    do_get
    response.should render_template('edit')
  end
  
  it "should find the algorithm requested" do
    Algorithm.should_receive(:find).and_return(@algorithm)
    do_get
  end
  
  it "should assign the found Algorithm for the view" do
    do_get
    assigns[:algorithm].should equal(@algorithm)
  end
end

describe AlgorithmsController, "handling POST /algorithms" do

  before do
    @algorithm = mock_model(Algorithm, :to_param => "1")
    Algorithm.stub!(:new).and_return(@algorithm)
  end
  
  def post_with_successful_save
    @algorithm.should_receive(:save).and_return(true)
    post :create, :algorithm => {}
  end
  
  def post_with_failed_save
    @algorithm.should_receive(:save).and_return(false)
    post :create, :algorithm => {}
  end
  
  it "should create a new algorithm" do
    Algorithm.should_receive(:new).with({}).and_return(@algorithm)
    post_with_successful_save
  end

  it "should redirect to the new algorithm on successful save" do
    post_with_successful_save
    response.should redirect_to(algorithm_url("1"))
  end

  it "should re-render 'new' on failed save" do
    post_with_failed_save
    response.should render_template('new')
  end
end

describe AlgorithmsController, "handling PUT /algorithms/1" do

  before do
    @algorithm = mock_model(Algorithm, :to_param => "1")
    Algorithm.stub!(:find).and_return(@algorithm)
  end
  
  def put_with_successful_update
    @algorithm.should_receive(:update_attributes).and_return(true)
    put :update, :id => "1"
  end
  
  def put_with_failed_update
    @algorithm.should_receive(:update_attributes).and_return(false)
    put :update, :id => "1"
  end
  
  it "should find the algorithm requested" do
    Algorithm.should_receive(:find).with("1").and_return(@algorithm)
    put_with_successful_update
  end

  it "should update the found algorithm" do
    put_with_successful_update
    assigns(:algorithm).should equal(@algorithm)
  end

  it "should assign the found algorithm for the view" do
    put_with_successful_update
    assigns(:algorithm).should equal(@algorithm)
  end

  it "should redirect to the algorithm on successful update" do
    put_with_successful_update
    response.should redirect_to(algorithm_url("1"))
  end

  it "should re-render 'edit' on failed update" do
    put_with_failed_update
    response.should render_template('edit')
  end
end

describe AlgorithmsController, "handling DELETE /algorithms/1" do

  before do
    @algorithm = mock_model(Algorithm, :destroy => true)
    Algorithm.stub!(:find).and_return(@algorithm)
  end
  
  def do_delete
    delete :destroy, :id => "1"
  end

  it "should find the algorithm requested" do
    Algorithm.should_receive(:find).with("1").and_return(@algorithm)
    do_delete
  end
  
  it "should call destroy on the found algorithm" do
    @algorithm.should_receive(:destroy)
    do_delete
  end
  
  it "should redirect to the algorithms list" do
    do_delete
    response.should redirect_to(algorithms_url)
  end
end
