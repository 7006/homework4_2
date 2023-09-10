-module(lesson3_task4_tests).

-include_lib("eunit/include/eunit.hrl").

decode_test_() ->
    [
        {
            "null",
            ?_assertEqual(
                null,
                lesson3_task4:decode(
                    <<"null">>,
                    proplist
                )
            )
        },
        {
            "true",
            ?_assertEqual(
                true,
                lesson3_task4:decode(
                    <<"true">>,
                    proplist
                )
            )
        },
        {
            "false",
            ?_assertEqual(
                false,
                lesson3_task4:decode(
                    <<"false">>,
                    proplist
                )
            )
        },
        {
            "number zero",
            ?_assertEqual(
                0,
                lesson3_task4:decode(
                    <<"0">>,
                    proplist
                )
            )
        },
        {
            "number integer",
            ?_assertEqual(
                925,
                lesson3_task4:decode(
                    <<"925">>,
                    proplist
                )
            )
        },
        {
            "number negative integer",
            ?_assertEqual(
                -541,
                lesson3_task4:decode(
                    <<"-541">>,
                    proplist
                )
            )
        },
        {
            "number float",
            ?_assertEqual(
                12.58,
                lesson3_task4:decode(
                    <<"12.58">>,
                    proplist
                )
            )
        },
        {
            "number negative float",
            ?_assertEqual(
                -1.23,
                lesson3_task4:decode(
                    <<"-1.23">>,
                    proplist
                )
            )
        },
        {
            "number fraction",
            ?_assertEqual(
                0.63,
                lesson3_task4:decode(
                    <<"0.63">>,
                    proplist
                )
            )
        },
        {
            "number negative fraction",
            ?_assertEqual(
                -0.82,
                lesson3_task4:decode(
                    <<"-0.82">>,
                    proplist
                )
            )
        },
        {
            "string empty",
            ?_assertEqual(
                <<>>,
                lesson3_task4:decode(
                    <<"''">>,
                    proplist
                )
            )
        },
        {
            "string lower",
            ?_assertEqual(
                <<"foobar">>,
                lesson3_task4:decode(
                    <<"'foobar'">>,
                    proplist
                )
            )
        },
        {
            "string upper",
            ?_assertEqual(
                <<"BARBAZ">>,
                lesson3_task4:decode(
                    <<"'BARBAZ'">>,
                    proplist
                )
            )
        },
        {
            "string digits",
            ?_assertEqual(
                <<"444">>,
                lesson3_task4:decode(
                    <<"'444'">>,
                    proplist
                )
            )
        },
        {
            "string mixed",
            ?_assertEqual(
                <<"4aBc9">>,
                lesson3_task4:decode(
                    <<"'4aBc9'">>,
                    proplist
                )
            )
        },
        {
            "string utf8",
            ?_assertEqual(
                <<"південь"/utf8>>,
                lesson3_task4:decode(
                    <<"'південь'"/utf8>>,
                    proplist
                )
            )
        },
        {
            "skip whitespace",
            ?_assertEqual(
                1,
                lesson3_task4:decode(
                    <<"       1">>,
                    proplist
                )
            )
        },
        {
            "array empty",
            ?_assertEqual(
                [],
                lesson3_task4:decode(
                    <<"[]">>,
                    proplist
                )
            )
        },
        {
            "array booleans",
            ?_assertEqual(
                [true, false, true],
                lesson3_task4:decode(
                    <<"[true, false, true]">>,
                    proplist
                )
            )
        },
        {
            "array null",
            ?_assertEqual(
                [null, null, null],
                lesson3_task4:decode(
                    <<"[null, null, null]">>,
                    proplist
                )
            )
        },
        {
            "array numbers",
            ?_assertEqual(
                [999, -20, 5.36, -108.99, 0, 0.81, -0.256],
                lesson3_task4:decode(
                    <<"[999, -20, 5.36, -108.99, 0, 0.81, -0.256]">>,
                    proplist
                )
            )
        },
        {
            "array strings",
            ?_assertEqual(
                [<<"abc">>, <<"DEF">>, <<"hIjK">>, <<"">>, <<"1111">>],
                lesson3_task4:decode(
                    <<"['abc', 'DEF', 'hIjK', '', '1111']">>,
                    proplist
                )
            )
        },
        {
            "array mixed",
            ?_assertEqual(
                [1, <<"foobar">>, <<"QuuX">>, 0.38, 0, false, true, null, -22, -0.5],
                lesson3_task4:decode(
                    <<"[1, 'foobar', 'QuuX', 0.38, 0, false, true, null, -22, -0.5]">>,
                    proplist
                )
            )
        },
        {
            "array nested",
            ?_assertEqual(
                [<<"foobar">>, [true, false], [null], 88],
                lesson3_task4:decode(
                    <<"['foobar', [true, false], [null], 88]">>,
                    proplist
                )
            )
        },
        {
            "array nested empty",
            ?_assertEqual(
                [[], [[]], [[[]]], [[[[]]]]],
                lesson3_task4:decode(
                    <<"[[], [[]], [[[]]], [[[[]]]]]">>,
                    proplist
                )
            )
        },
        {
            "array nested object",
            ?_assertEqual(
                [
                    [
                        {<<"a">>, 1},
                        {<<"b">>, 2}
                    ],
                    [
                        {<<"x">>, <<"xxxx">>},
                        {<<"y">>, <<"yyy">>}
                    ]
                ],
                lesson3_task4:decode(
                    <<"[ {'a': 1, 'b': 2}, {'x': 'xxxx', 'y': 'yyy'} ]">>,
                    proplist
                )
            )
        },
        {
            "object empty",
            ?_assertEqual(
                [],
                lesson3_task4:decode(
                    <<"{}">>,
                    proplist
                )
            )
        },
        {
            "object 1 pair",
            ?_assertEqual(
                [
                    {<<"enabled">>, true}
                ],
                lesson3_task4:decode(
                    <<"{'enabled': true}">>,
                    proplist
                )
            )
        },
        {
            "object 2 pairs",
            ?_assertEqual(
                [
                    {<<"enabled">>, false},
                    {<<"is_new">>, true}
                ],
                lesson3_task4:decode(
                    <<"{'enabled': false, 'is_new': true}">>,
                    proplist
                )
            )
        },
        {
            "object many pairs",
            ?_assertEqual(
                [
                    {<<"available">>, true},
                    {<<"model">>, <<"T-1000">>},
                    {<<"parts">>, null},
                    {<<"release_year">>, 1994},
                    {<<"price">>, 10.27},
                    {<<"rating">>, 0.8}
                ],
                lesson3_task4:decode(
                    <<"{'available': true, 'model': 'T-1000', 'parts': null, 'release_year': 1994, 'price': 10.27, 'rating': 0.8}">>,
                    proplist
                )
            )
        },
        {
            "object array pairs",
            ?_assertEqual(
                [
                    {<<"task_id">>, 4758},
                    {<<"subtask_ids">>, [1084, 1102, 1103]},
                    {<<"affected_releases">>, [<<"BGT/12">>, <<"LAG/2">>]}
                ],
                lesson3_task4:decode(
                    <<"{'task_id': 4758, 'subtask_ids': [1084, 1102, 1103], 'affected_releases': ['BGT/12', 'LAG/2']}">>,
                    proplist
                )
            )
        },
        {
            "object nested object pairs",
            ?_assertEqual(
                [
                    {<<"a">>, 1},
                    {<<"b">>, [
                        {<<"aa">>, true},
                        {<<"bb">>, [
                            {<<"aaa">>, 4.23},
                            {<<"bbb">>, [
                                {<<"aaaa">>, <<"cccc">>},
                                {<<"bbbb">>, [
                                    {<<"aaaaa">>, []},
                                    {<<"bbbbb">>, []}
                                ]}
                            ]}
                        ]}
                    ]}
                ],
                lesson3_task4:decode(
                    <<"{'a': 1, 'b': {'aa': true, 'bb': {'aaa': 4.23, 'bbb': {'aaaa': 'cccc', 'bbbb': {'aaaaa': [], 'bbbbb': {} }}}}}">>,
                    proplist
                )
            )
        },
        {
            "squad",
            ?_assertEqual(
                [
                    {<<"squadName">>, <<"Super hero squad">>},
                    {<<"homeTown">>, <<"Metro City">>},
                    {<<"formed">>, 2016},
                    {<<"secretBase">>, <<"Super tower">>},
                    {<<"active">>, true},
                    {<<"members">>, [
                        [
                            {<<"name">>, <<"Molecule Man">>},
                            {<<"age">>, 29},
                            {<<"secretIdentity">>, <<"Dan Jukes">>},
                            {<<"powers">>, [
                                <<"Radiation resistance">>,
                                <<"Turning tiny">>,
                                <<"Radiation blast">>
                            ]}
                        ],
                        [
                            {<<"name">>, <<"Madame Uppercut">>},
                            {<<"age">>, 39},
                            {<<"secretIdentity">>, <<"Jane Wilson">>},
                            {<<"powers">>, [
                                <<"Million tonne punch">>,
                                <<"Damage resistance">>,
                                <<"Superhuman reflexes">>
                            ]}
                        ],
                        [
                            {<<"name">>, <<"Eternal Flame">>},
                            {<<"age">>, 1000000},
                            {<<"secretIdentity">>, <<"Unknown">>},
                            {<<"powers">>, [
                                <<"Immortality">>,
                                <<"Heat Immunity">>,
                                <<"Inferno">>,
                                <<"Teleportation">>,
                                <<"Interdimensional travel">>
                            ]}
                        ]
                    ]}
                ],
                lesson3_task4:decode(
                    <<"{'squadName': 'Super hero squad', 'homeTown': 'Metro City', 'formed': 2016, 'secretBase': 'Super tower', 'active': true,  'members': [{'name': 'Molecule Man', 'age': 29, 'secretIdentity': 'Dan Jukes', 'powers': ['Radiation resistance', 'Turning tiny', 'Radiation blast']}, {'name': 'Madame Uppercut', 'age': 39, 'secretIdentity': 'Jane Wilson', 'powers': ['Million tonne punch', 'Damage resistance', 'Superhuman reflexes']},{'name': 'Eternal Flame', 'age': 1000000, 'secretIdentity': 'Unknown', 'powers': ['Immortality', 'Heat Immunity', 'Inferno', 'Teleportation', 'Interdimensional travel']}]}">>,
                    proplist
                )
            )
        },
        {
            "array nested object (map)",
            ?_assertEqual(
                [
                    #{
                        <<"a">> => 1,
                        <<"b">> => 2
                    },
                    #{
                        <<"x">> => <<"xxxx">>,
                        <<"y">> => <<"yyy">>
                    }
                ],
                lesson3_task4:decode(
                    <<"[ {'a': 1, 'b': 2}, {'x': 'xxxx', 'y': 'yyy'} ]">>,
                    map
                )
            )
        },
        {
            "array empty object (map)",
            ?_assertEqual(
                #{},
                lesson3_task4:decode(
                    <<"{}">>,
                    map
                )
            )
        },
        {
            "object 1 pair (map)",
            ?_assertEqual(
                #{
                    <<"enabled">> => true
                },
                lesson3_task4:decode(
                    <<"{'enabled': true}">>,
                    map
                )
            )
        },
        {
            "object 2 pairs (map)",
            ?_assertEqual(
                #{
                    <<"enabled">> => false,
                    <<"is_new">> => true
                },
                lesson3_task4:decode(
                    <<"{'enabled': false, 'is_new': true}">>,
                    map
                )
            )
        },
        {
            "object many pairs (map)",
            ?_assertEqual(
                #{
                    <<"available">> => true,
                    <<"model">> => <<"T-1000">>,
                    <<"parts">> => null,
                    <<"release_year">> => 1994,
                    <<"price">> => 10.27,
                    <<"rating">> => 0.8
                },
                lesson3_task4:decode(
                    <<"{'available': true, 'model': 'T-1000', 'parts': null, 'release_year': 1994, 'price': 10.27, 'rating': 0.8}">>,
                    map
                )
            )
        },
        {
            "object array pairs (map)",
            ?_assertEqual(
                #{
                    <<"task_id">> => 4758,
                    <<"subtask_ids">> => [1084, 1102, 1103],
                    <<"affected_releases">> => [<<"BGT/12">>, <<"LAG/2">>]
                },
                lesson3_task4:decode(
                    <<"{'task_id': 4758, 'subtask_ids': [1084, 1102, 1103], 'affected_releases': ['BGT/12', 'LAG/2']}">>,
                    map
                )
            )
        },
        {
            "object nested object pairs (map)",
            ?_assertEqual(
                #{
                    <<"a">> => 1,
                    <<"b">> => #{
                        <<"aa">> => true,
                        <<"bb">> => #{
                            <<"aaa">> => 4.23,
                            <<"bbb">> => #{
                                <<"aaaa">> => <<"cccc">>,
                                <<"bbbb">> => #{
                                    <<"aaaaa">> => [],
                                    <<"bbbbb">> => #{}
                                }
                            }
                        }
                    }
                },
                lesson3_task4:decode(
                    <<"{'a': 1, 'b': {'aa': true, 'bb': {'aaa': 4.23, 'bbb': {'aaaa': 'cccc', 'bbbb': {'aaaaa': [], 'bbbbb': {} }}}}}">>,
                    map
                )
            )
        },
        {
            "squad (map)",
            ?_assertEqual(
                #{
                    <<"squadName">> => <<"Super hero squad">>,
                    <<"homeTown">> => <<"Metro City">>,
                    <<"formed">> => 2016,
                    <<"secretBase">> => <<"Super tower">>,
                    <<"active">> => true,
                    <<"members">> => [
                        #{
                            <<"name">> => <<"Molecule Man">>,
                            <<"age">> => 29,
                            <<"secretIdentity">> => <<"Dan Jukes">>,
                            <<"powers">> => [
                                <<"Radiation resistance">>,
                                <<"Turning tiny">>,
                                <<"Radiation blast">>
                            ]
                        },
                        #{
                            <<"name">> => <<"Madame Uppercut">>,
                            <<"age">> => 39,
                            <<"secretIdentity">> => <<"Jane Wilson">>,
                            <<"powers">> => [
                                <<"Million tonne punch">>,
                                <<"Damage resistance">>,
                                <<"Superhuman reflexes">>
                            ]
                        },
                        #{
                            <<"name">> => <<"Eternal Flame">>,
                            <<"age">> => 1000000,
                            <<"secretIdentity">> => <<"Unknown">>,
                            <<"powers">> => [
                                <<"Immortality">>,
                                <<"Heat Immunity">>,
                                <<"Inferno">>,
                                <<"Teleportation">>,
                                <<"Interdimensional travel">>
                            ]
                        }
                    ]
                },
                lesson3_task4:decode(
                    <<"{'squadName': 'Super hero squad', 'homeTown': 'Metro City', 'formed': 2016, 'secretBase': 'Super tower', 'active': true,  'members': [{'name': 'Molecule Man', 'age': 29, 'secretIdentity': 'Dan Jukes', 'powers': ['Radiation resistance', 'Turning tiny', 'Radiation blast']}, {'name': 'Madame Uppercut', 'age': 39, 'secretIdentity': 'Jane Wilson', 'powers': ['Million tonne punch', 'Damage resistance', 'Superhuman reflexes']},{'name': 'Eternal Flame', 'age': 1000000, 'secretIdentity': 'Unknown', 'powers': ['Immortality', 'Heat Immunity', 'Inferno', 'Teleportation', 'Interdimensional travel']}]}">>,
                    map
                )
            )
        },
        {
            "whitespaces at the end of the text",
            ?_assertEqual(
                [{<<"a">>, <<"ccc">>}],
                lesson3_task4:decode(
                    <<$\n, "   {'a'", $\n, ":", $\n, "'ccc'", $\n, "     }", $\n>>,
                    proplist
                )
            )
        },
        {
            "whitespaces at the end of the text (map)",
            ?_assertEqual(
                #{
                    <<"a">> => <<"ccc">>
                },
                lesson3_task4:decode(
                    <<$\n, "   {'a'", $\n, ":", $\n, "'ccc'", $\n, "     }", $\n>>,
                    map
                )
            )
        }
    ].
