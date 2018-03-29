from copy import deepcopy
from collections import OrderedDict

from quant_sim.stats_mgmt.statistics import Statistics
from quant_sim.order_mgmt.order_mngr import Order_Manager
from quant_sim.environment import Environment
from quant_sim.errors import Date_Not_In_History_Error

class Algorithm(object):
    """
    Base class for trading algorithms. Inherit and overload
    initialize() and process_data().

    Parameters
    ---------- 
    id : str , required
    desc : str , default 'description n/a'
    delay_start : int, optional, default 0
             used to delay the processing of data by the algorithm
             useful when an algorithm needs to access x data in past, which
             may or may not exist yet.
             i.e. eod[4] even though the data only goes back to eod[2]. Algorithm
                 could use delay_start=4 to prevent any key errors being thrown

    Returns
    -------
    n/a

    See Also
    --------
    quant.examples.example_algo
    quant.example.sexample_suite

    Notes
    -----
    self.delay_start is not needed majority of time.
    If SPY data goes back to 1993-1-1, and simulation starts on 1993-1-2 and
    algorithm checks eod[4].c > eod[0].c then an error would be thrown

    Examples
    --------
    class MyAlgo(TradingAlgorithm):
        def initialize(amount, *args, **kwargs):
            self.shares = amount

        def process_data(env):
            eod = env['eod']['SPY']
            self.order(eod, self.sharess)
    """
    def __init__(self, *args, **kwargs):
        self.id = kwargs.get('id','')
        self.desc = kwargs.get('desc','description n/a')
        self.delay_start = kwargs.get('delay_start',0)
        self.ignore_old = kwargs.get('ignore_old',True)
        self.n = 0
        self.order_mngr = Order_Manager(*args, **kwargs)
        self.stats_mngr = Statistics(*args, **kwargs)
        self.metrics = Environment()
        self.metrics.funcs = OrderedDict()
        self.now_dt = None
        self.last_now = None
        self.recorded_keys = None
        self.last_recorded_dt = None
        self.initialize(*args, **kwargs)
    
    def initialize_recorder(self, recorded_vars, cache=False, fn=''):
        """
        Initializes recording functionality for algorithm.
        
        Parameters
        ---------- 
        recorded_vars : list or dict , required
                use a list to initialize record() from algorithm.initialize()
                The list should contain keys of the variables to store in the
                order they should be listed in the record file where they are
                used as headers.
                Use a dict from within algorithm.process_data() where values
                are keyed to the same key names used in initialization.
        cache : bool, optional, default=False
                used to determine if recorded variables should be stored in history
                Maintaining a history of recorded variables is discouraged as it
                uses a lot of memory and is not necessary. If you absolutely need to
                keep track of a variable in history, use a metric
        fn : string, optional, default=''
                filename where recorded variables should be stored
        
        Returns
        -------
        sets up self.recorded_keys and self.recorded and the output file
 
        See Also
        --------
        n/a
        
        Notes
        -----
        n/a
        
        Examples
        --------
        class MyAlgo(TradingAlgorithm):
            def initialize(amount, *args, **kwargs):
                self.record(['gap_size', 'open_price', 'action'])
                self.initialize_recorder(['gap_size', 'open_price', 'action'], cache=False, fn='myDir/records.csv')
        """
        self.recorded = OrderedDict()
        self.record_fn = fn if fn != '' else self.id+ '_record.csv'
        with open(self.record_fn, 'w') as f:
            f.write('Date,'+','.join(recorded_vars)+'\n')
        self.recorded_keys = recorded_vars
        self.cache_recorded = cache
        
    def update_strat(self, env, *args, **kwargs):
        self.now_dt = env.now_dt
        if self.n < self.delay_start: 
            self.last_now = env.now_dt
            return
        self.order_mngr.update(env)
        try: 
            self.process_data(env)
        except Date_Not_In_History_Error as e:
            if self.ignore_old:
                return
            else:
                raise e
        self.last_now = env.now_dt
        self.stats_mngr.update(env,self.order_mngr.closed_pos[self.stats_mngr.n:])
        if self.recorded_keys != None:
            self.record({})

    def add_metric(self, metrics):
        if type(metrics) != list:
            metrics = [metrics]
        for m in metrics:
            self.metrics.funcs[m.id] = m
            self.metrics.add_data(m.id, OrderedDict())
            
    def update_metrics(self, env):
        self.n += 1
        self.metrics.update(env.now_dt)
        for k,m in self.metrics.funcs.items():
            self.metrics.data[m.id][env.now_dt] = m.update(env)

    def order(self, sid, shares, price, **kwargs):
        """
        API
        front end order_processer for quant.finances.algorith.process_data
        this is only order method a user is allowed to access
        """
        self.order_mngr.order(sid, shares, price, **kwargs)
    

    def record(self, recorded_vars):
        """
        Adds recording functionality to algorithms. Can record any number of 
        self defined variables. Can be called anywhere within algorithm
        All variables will be stored according to date they were generated.
    
        Parameters
        ---------- 
        recorded_vars : list or dict , required
                Use a dict from within algorithm.process_data() where values
                are keyed to the same key names used in initialization.

        Returns
        -------
        saves all recorded values to self.recorded, a dict keyed by datetime.
        self.recorded is cleared out at each new day based on self.cache_recorded
        Appends all recorded values in a csv file format to self.recorded_fn
        
        See Also
        --------
        n/a
        
        Notes
        -----

    
        Examples
        --------
        class MyAlgo(TradingAlgorithm):
            def initialize(amount, *args, **kwargs):
                self.initialize_recorder(['gap_size', 'open_price', 'action'], False, fn='mydir/record.csv')
            def process_data(env):
                eod = env['eod']['SPY']
                self.record({'gap_size': eod.o - eod[1].c, 'open_price': eod.o})
                self.record({'action': 'buy'})
        """
        if len(recorded_vars) == 0 and self.last_recorded_dt in self.recorded:
            f = open(self.record_fn, 'a')
            f.write('%s,'%(self.last_recorded_dt))
            for k in self.recorded_keys:
                f.write('%s,' % self.recorded[self.last_recorded_dt].get(k,''))
            f.write('\n')
            f.close()
            if not self.cache_recorded:
                self.recorded.pop(self.last_recorded_dt)
            self.last_recorded_dt = None
        elif len(recorded_vars) == 0:
            return
        else:
            if self.now_dt not in self.recorded:
                self.recorded[self.now_dt] = OrderedDict()
            for k, v in recorded_vars.items():
                self.recorded[self.now_dt][k] = v
            self.last_recorded_dt = self.now_dt
            

    def initialize(self, *args, **kwargs):
        """
        All Algorithm administrative actions take place here.
        An example of things that should be done in initiliaze() are but not
        limited to:
            self.id = 'algorithm_title
            self.desc = 'algorithm description'
            self.add_metric()
            initialize_recorder()
            self.delay_start = 4
            
        Parameters
        ---------- 
        *args / **kwargs : any , optional
              passed down from simulator

        Returns
        -------
        n/a

        See Also
        --------
        n/a

        Notes
        -----
        n/a

        Examples
        --------
        class MyAlgo(TradingAlgorithm):
            def initialize(amount, *args, **kwargs):
                self.id = 'myalgo'
                self.desc = 'My super awesome algorithm'
                self.add_metric(Min(id='8daylow',window=8,func=lambda env: env.c),['SPY'])
                self.delay_start = 3
                self.initialize_recorder(['gap_size', 'open_price', 'action'])
        """
        pass

    def process_data(self, env, *args, **kwargs):
        pass