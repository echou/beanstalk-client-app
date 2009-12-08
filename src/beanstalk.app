% vim:syn=erlang
{application, beanstalk,
    [   {description, "beanstalk"},
        {vsn, "1.0"},
        {modules, []},
        {registered, []},
        {mod, {beanstalk_app, []}},
        {applications, [kernel,stdlib]},
        {env, [
            {tubes, [
                {'bbs-req', ["127.0.0.1:11300"], 1, 2}
            ]}
        ]}
    ]
}.

