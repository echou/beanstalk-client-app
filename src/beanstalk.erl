-module(beanstalk).

-compile(export_all).


start() ->
    beanstalk_app:start().


put(Tube, Data) ->
    beanstalk_client:call(Tube, producer, {put, Data, []}).    


