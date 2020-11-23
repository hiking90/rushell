mod ast;
mod lexer;

lalrpop_mod!(pub posix_parser, "/program/parser.rs"); // synthesized by LALRPOP


#[allow(unused)]
macro_rules! literal_word_vec {
    ($($x:expr), *) => {
        vec![$( ast::Word(vec![ast::Span::Literal($x.to_string())]), )*]
    };
}

#[allow(unused)]
macro_rules! lit {
    ($x:expr) => {
        ast::Word(vec![ast::Span::Literal($x.to_string())])
    };
}

#[allow(unused)]
macro_rules! param {
    ($name:expr, $op:expr, $quoted:expr) => {
        ast::Word(vec![ast::Span::Parameter {
            name: $name.to_string(),
            op: $op,
            quoted: $quoted,
        }])
    };
}

#[allow(unused)]
macro_rules! parse {
    ($errors:expr, $x:expr) => {
        posix_parser::ProgramParser::new().parse($errors, lexer::Lexer::new($x));
        // posix_parser::ProgramParser::new().parse($errors, $x)
    };
}

#[test]
fn test_temporary() {
    // let mut errors = Vec::new();
    // let res = parse!(&mut errors, "echo \\\nreachable&");
    // panic!("{:?}", res);
}

#[test]
fn test_simple_commands() {
    let mut errors = Vec::new();
    assert_eq!(
        parse!(&mut errors, "ls -G /tmp\n"),
        Ok(ast::Program {
            terms: vec![ast::Term {
                background: false,
                pipelines: vec![ast::Pipeline {
                    run_if: ast::RunIf::Always,
                    commands: vec![ast::Command::SimpleCommand {
                        external: false,
                        argv: literal_word_vec!["ls", "-G", "/tmp"],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parse!(&mut errors, "echo hello | hexdump -C | date"),
        Ok(ast::Program {
            terms: vec![ast::Term {
                background: false,
                pipelines: vec![ast::Pipeline {
                    run_if: ast::RunIf::Always,
                    commands: vec![
                        ast::Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "hello"],
                            redirects: vec![],
                            assignments: vec![],
                        },
                        ast::Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["hexdump", "-C"],
                            redirects: vec![],
                            assignments: vec![],
                        },
                        ast::Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["date"],
                            redirects: vec![],
                            assignments: vec![],
                        },
                    ],
                }],
            }],
        })
    );

    assert_eq!(
        parse!(&mut errors, "false || false && echo unreachable; echo \\\nreachable"),
        Ok(ast::Program {
            terms: vec![
                ast::Term {
                    background: false,
                    pipelines: vec![
                        ast::Pipeline {
                            run_if: ast::RunIf::Always,
                            commands: vec![ast::Command::SimpleCommand {
                                external: false,
                                argv: literal_word_vec!["false"],
                                redirects: vec![],
                                assignments: vec![],
                            }],
                        },
                        ast::Pipeline {
                            run_if: ast::RunIf::Failure,
                            commands: vec![ast::Command::SimpleCommand {
                                external: false,
                                argv: literal_word_vec!["false"],
                                redirects: vec![],
                                assignments: vec![],
                            }],
                        },
                        ast::Pipeline {
                            run_if: ast::RunIf::Success,
                            commands: vec![ast::Command::SimpleCommand {
                                external: false,
                                argv: literal_word_vec!["echo", "unreachable"],
                                redirects: vec![],
                                assignments: vec![],
                            }],
                        },
                    ],
                },
                ast::Term {
                    background: false,
                    pipelines: vec![ast::Pipeline {
                        run_if: ast::RunIf::Always,
                        commands: vec![ast::Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "reachable"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        })
    );

    assert_eq!(
        parse!(&mut errors, "echo -n \"Hello world\" from; echo nsh"),
        Ok(ast::Program {
            terms: vec![
                ast::Term {
                    background: false,
                    pipelines: vec![ast::Pipeline {
                        run_if: ast::RunIf::Always,
                        commands: vec![ast::Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "-n", "Hello world", "from"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                ast::Term {
                    background: false,
                    pipelines: vec![ast::Pipeline {
                        run_if: ast::RunIf::Always,
                        commands: vec![ast::Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "nsh"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        })
    );


    assert_eq!(
        parse!(&mut errors, "echo foo & sleep 1 &\n echo bar; echo baz ; echo foo2 &"),
        Ok(ast::Program {
            terms: vec![
                ast::Term {
                    // code: "echo foo".into(),
                    background: true,
                    pipelines: vec![ast::Pipeline {
                        run_if: ast::RunIf::Always,
                        commands: vec![ast::Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "foo"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                ast::Term {
                    // code: "sleep 1".into(),
                    background: true,
                    pipelines: vec![ast::Pipeline {
                        run_if: ast::RunIf::Always,
                        commands: vec![ast::Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["sleep", "1"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                ast::Term {
                    // code: "echo bar".into(),
                    background: false,
                    pipelines: vec![ast::Pipeline {
                        run_if: ast::RunIf::Always,
                        commands: vec![ast::Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "bar"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                ast::Term {
                    // code: "echo baz".into(),
                    background: false,
                    pipelines: vec![ast::Pipeline {
                        run_if: ast::RunIf::Always,
                        commands: vec![ast::Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "baz"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                ast::Term {
                    // code: "echo foo2".into(),
                    background: true,
                    pipelines: vec![ast::Pipeline {
                        run_if: ast::RunIf::Always,
                        commands: vec![ast::Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "foo2"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        }),
    );


    assert_eq!(
        parse!(&mut errors, "PORT=1234 RAILS_ENV=production rails s"),
        Ok(ast::Program {
            terms: vec![ast::Term {
                // code: "PORT=1234 RAILS_ENV=production rails s".into(),
                background: false,
                pipelines: vec![ast::Pipeline {
                    run_if: ast::RunIf::Always,
                    commands: vec![ast::Command::SimpleCommand {
                        external: false,
                        argv: literal_word_vec!["rails", "s"],
                        redirects: vec![],
                        assignments: vec![
                            ast::Assignment {
                                name: "PORT".into(),
                                initializer: ast::Initializer::String(ast::Word(vec![ast::Span::Literal(
                                    "1234".into()
                                )])),
                                index: None,
                                append: false,
                            },
                            ast::Assignment {
                                name: "RAILS_ENV".into(),
                                initializer: ast::Initializer::String(ast::Word(vec![ast::Span::Literal(
                                    "production".into()
                                )])),
                                index: None,
                                append: false,
                            }
                        ],
                    }],
                }],
            }],
        })
    );


    assert_eq!(
        parse!(&mut errors, "ls -G <foo.txt >> bar.txt 2> baz.txt 4>&2"),
        Ok(ast::Program {
            terms: vec![ast::Term {
                // code: "ls -G <foo.txt >> bar.txt 2> baz.txt 4>&2".into(),
                background: false,
                pipelines: vec![ast::Pipeline {
                    run_if: ast::RunIf::Always,
                    commands: vec![ast::Command::SimpleCommand {
                        external: false,
                        argv: literal_word_vec!["ls", "-G"],
                        redirects: vec![
                            ast::Redirection {
                                direction: ast::RedirectionDirection::Input,
                                fd: 0,
                                target: ast::RedirectionType::File(lit!("foo.txt")),
                            },
                            ast::Redirection {
                                direction: ast::RedirectionDirection::Append,
                                fd: 1,
                                target: ast::RedirectionType::File(lit!("bar.txt")),
                            },
                            ast::Redirection {
                                direction: ast::RedirectionDirection::Output,
                                fd: 2,
                                target: ast::RedirectionType::File(lit!("baz.txt")),
                            },
                            ast::Redirection {
                                direction: ast::RedirectionDirection::Output,
                                fd: 4,
                                target: ast::RedirectionType::Fd(2),
                            },
                        ],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );
}


#[test]
pub fn test_compound_commands() {
    let mut errors = Vec::new();

    assert_eq!(
        parse!(&mut errors, "if true; then echo it works; fi"),
        Ok(ast::Program {
            terms: vec![ast::Term {
                // code: "if true; then echo it works; fi".into(),
                pipelines: vec![ast::Pipeline {
                    run_if: ast::RunIf::Always,
                    commands: vec![ast::Command::If {
                        condition: vec![ast::Term {
                            // code: "true".into(),
                            pipelines: vec![ast::Pipeline {
                                run_if: ast::RunIf::Always,
                                commands: vec![ast::Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["true"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        then_part: vec![ast::Term {
                            // code: "echo it works".into(),
                            pipelines: vec![ast::Pipeline {
                                run_if: ast::RunIf::Always,
                                commands: vec![ast::Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["echo", "it", "works"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        elif_parts: vec![],
                        else_part: None,
                        redirects: vec![],
                    }],
                }],
                background: false,
            }],
        })
    );

    assert_eq!(
        parse!(&mut errors, "while maybe-true;\ndo\n echo \"while loop!\"; done"),
        Ok(ast::Program {
            terms: vec![ast::Term {
                // code: "while maybe-true;\ndo\n echo \"while loop!\"; done".into(),
                pipelines: vec![ast::Pipeline {
                    run_if: ast::RunIf::Always,
                    commands: vec![ast::Command::While {
                        condition: vec![ast::Term {
                            // code: "maybe-true".into(),
                            pipelines: vec![ast::Pipeline {
                                run_if: ast::RunIf::Always,
                                commands: vec![ast::Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["maybe-true"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        body: vec![ast::Term {
                            // code: "echo \"while loop!\"".into(),
                            pipelines: vec![ast::Pipeline {
                                run_if: ast::RunIf::Always,
                                commands: vec![ast::Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["echo", "while loop!"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                    }],
                }],
                background: false,
            }],
        })
    );

    assert_eq!(
        parse!(&mut errors, concat!(
            "if [ foo = \"foo\" ];\n",
            "then\n",
            "    echo hello\n",
            "    echo world\n",
            "fi"
        )),
        Ok(ast::Program {
            terms: vec![ast::Term {
                // code: "if [ foo = \"foo\" ];\nthen\n    echo hello\n    echo world\nfi".into(),
                pipelines: vec![ast::Pipeline {
                    run_if: ast::RunIf::Always,
                    commands: vec![ast::Command::If {
                        condition: vec![ast::Term {
                            // code: "[ foo = \"foo\" ]".into(),
                            pipelines: vec![ast::Pipeline {
                                run_if: ast::RunIf::Always,
                                commands: vec![ast::Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["[", "foo", "=", "foo", "]"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        then_part: vec![
                            ast::Term {
                                // code: "echo hello".into(),
                                pipelines: vec![ast::Pipeline {
                                    run_if: ast::RunIf::Always,
                                    commands: vec![ast::Command::SimpleCommand {
                                        external: false,
                                        argv: literal_word_vec!["echo", "hello"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                                background: false,
                            },
                            ast::Term {
                                // code: "echo world".into(),
                                pipelines: vec![ast::Pipeline {
                                    run_if: ast::RunIf::Always,
                                    commands: vec![ast::Command::SimpleCommand {
                                        external: false,
                                        argv: literal_word_vec!["echo", "world"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                                background: false,
                            },
                        ],
                        elif_parts: vec![],
                        else_part: None,
                        redirects: vec![],
                    }],
                }],
                background: false,
            }],
        })
    );

    // assert_eq!(
    //     parse(concat!(
    //         "if [ $name = \"john\" ];",
    //         "then;",
    //         "    echo Hello, John!;",
    //         "elif [ $name = \"mike\" ];",
    //         "then;",
    //         "    echo Hello, Mike!;",
    //         "elif [ $name = \"emily\" ];",
    //         "then;",
    //         "    echo Hello, Emily!;",
    //         "else;",
    //         "    echo Hello, stranger!;",
    //         "fi"
    //     )),
    //     Ok(Ast {
    //         terms: vec![Term {
    //             code: "if [ $name = \"john\" ];then;    echo Hello, John!;elif [ $name = \"mike\" ];then;    echo Hello, Mike!;elif [ $name = \"emily\" ];then;    echo Hello, Emily!;else;    echo Hello, stranger!;fi".into(),
    //             pipelines: vec![Pipeline {
    //                 run_if: RunIf::Always,
    //                 commands: vec![Command::If {
    //                     condition: vec![Term {
    //                         code: "[ $name = \"john\" ]".into(),
    //                         pipelines: vec![Pipeline {
    //                             run_if: RunIf::Always,
    //                             commands: vec![Command::SimpleCommand {
    //                                 external: false,
    //                                 argv: vec![
    //                                     lit!("["),
    //                                     param!("name", ExpansionOp::GetOrEmpty, false),
    //                                     lit!("="),
    //                                     lit!("john"),
    //                                     lit!("]"),
    //                                 ],
    //                                 redirects: vec![],
    //                                 assignments: vec![],
    //                             }],
    //                         }],
    //                         background: false,
    //                     }],
    //                     then_part: vec![Term {
    //                         code: "echo Hello, John!".into(),
    //                         pipelines: vec![Pipeline {
    //                             run_if: RunIf::Always,
    //                             commands: vec![Command::SimpleCommand {
    //                                 external: false,
    //                                 argv: literal_word_vec!["echo", "Hello,", "John!"],
    //                                 redirects: vec![],
    //                                 assignments: vec![],
    //                             }],
    //                         }],
    //                         background: false,
    //                     }],
    //                     elif_parts: vec![
    //                         ElIf {
    //                             condition: vec![Term {
    //                                 code: "[ $name = \"mike\" ]".into(),
    //                                 pipelines: vec![Pipeline {
    //                                     run_if: RunIf::Always,
    //                                     commands: vec![Command::SimpleCommand {
    //                                         external: false,
    //                                         argv: vec![
    //                                             lit!("["),
    //                                             param!("name", ExpansionOp::GetOrEmpty, false),
    //                                             lit!("="),
    //                                             lit!("mike"),
    //                                             lit!("]"),
    //                                         ],
    //                                         redirects: vec![],
    //                                         assignments: vec![],
    //                                     }],
    //                                 }],
    //                                 background: false,
    //                             }],
    //                             then_part: vec![Term {
    //                                 code: "echo Hello, Mike!".into(),
    //                                 pipelines: vec![Pipeline {
    //                                     run_if: RunIf::Always,
    //                                     commands: vec![Command::SimpleCommand {
    //                                         external: false,
    //                                         argv: literal_word_vec!["echo", "Hello,", "Mike!"],
    //                                         redirects: vec![],
    //                                         assignments: vec![],
    //                                     }],
    //                                 }],
    //                                 background: false,
    //                             }],
    //                         },
    //                         ElIf {
    //                             condition: vec![Term {
    //                                 code: "[ $name = \"emily\" ]".into(),
    //                                 pipelines: vec![Pipeline {
    //                                     run_if: RunIf::Always,
    //                                     commands: vec![Command::SimpleCommand {
    //                                         external: false,
    //                                         argv: vec![
    //                                             lit!("["),
    //                                             param!("name", ExpansionOp::GetOrEmpty, false),
    //                                             lit!("="),
    //                                             lit!("emily"),
    //                                             lit!("]"),
    //                                         ],
    //                                         redirects: vec![],
    //                                         assignments: vec![],
    //                                     }],
    //                                 }],
    //                                 background: false,
    //                             }],
    //                             then_part: vec![Term {
    //                                 code: "echo Hello, Emily!".into(),
    //                                 pipelines: vec![Pipeline {
    //                                     run_if: RunIf::Always,
    //                                     commands: vec![Command::SimpleCommand {
    //                                         external: false,
    //                                         argv: literal_word_vec!["echo", "Hello,", "Emily!"],
    //                                         redirects: vec![],
    //                                         assignments: vec![],
    //                                     }],
    //                                 }],
    //                                 background: false,
    //                             }],
    //                         },
    //                     ],
    //                     else_part: Some(vec![Term {
    //                         code: "echo Hello, stranger!".into(),
    //                         pipelines: vec![Pipeline {
    //                             run_if: RunIf::Always,
    //                             commands: vec![Command::SimpleCommand {
    //                                 external: false,
    //                                 argv: literal_word_vec!["echo", "Hello,", "stranger!"],
    //                                 redirects: vec![],
    //                                 assignments: vec![],
    //                             }],
    //                         }],
    //                         background: false,
    //                     }]),
    //                     redirects: vec![],
    //                 }],
    //             }],
    //             background: false,
    //         }],
    //     })
    // );

    // assert_eq!(
    //     parse("for arg in hello world; do echo ---------; cowsay $arg; done"),
    //     Ok(Ast {
    //         terms: vec![Term {
    //             code: "for arg in hello world; do echo ---------; cowsay $arg; done".into(),
    //             background: false,
    //             pipelines: vec![Pipeline {
    //                 run_if: RunIf::Always,
    //                 commands: vec![Command::For {
    //                     var_name: "arg".into(),
    //                     words: literal_word_vec!["hello", "world"],
    //                     body: vec![
    //                         Term {
    //                             code: "echo ---------".into(),
    //                             background: false,
    //                             pipelines: vec![Pipeline {
    //                                 run_if: RunIf::Always,
    //                                 commands: vec![Command::SimpleCommand {
    //                                     external: false,
    //                                     argv: literal_word_vec!["echo", "---------"],
    //                                     redirects: vec![],
    //                                     assignments: vec![],
    //                                 }],
    //                             }],
    //                         },
    //                         Term {
    //                             code: "cowsay $arg".into(),
    //                             background: false,
    //                             pipelines: vec![Pipeline {
    //                                 run_if: RunIf::Always,
    //                                 commands: vec![Command::SimpleCommand {
    //                                     external: false,
    //                                     argv: vec![
    //                                         lit!("cowsay"),
    //                                         param!("arg", ExpansionOp::GetOrEmpty, false),
    //                                     ],
    //                                     redirects: vec![],
    //                                     assignments: vec![],
    //                                 }],
    //                             }],
    //                         },
    //                     ],
    //                 }],
    //             }],
    //         }],
    //     })
    // );

    // assert_eq!(
    //     parse(concat!(
    //         "for arg in hello world; do",
    //         "   if sometimes-true; then\n",
    //         "       break\n",
    //         "   fi\n",
    //         "   if sometimes-true; then\n",
    //         "       continue;\n",
    //         "   fi\n",
    //         "   something &\n",
    //         "done"
    //     )),
    //     Ok(Ast {
    //         terms: vec![Term {
    //             code: "for arg in hello world; do   if sometimes-true; then\n       break\n   fi\n   if sometimes-true; then\n       continue;\n   fi\n   something &\ndone".into(),
    //             background: false,
    //             pipelines: vec![Pipeline {
    //                 run_if: RunIf::Always,
    //                 commands: vec![Command::For {
    //                     var_name: "arg".into(),
    //                     words: literal_word_vec!["hello", "world"],
    //                     body: vec![
    //                         Term {
    //                             code: "if sometimes-true; then\n       break\n   fi".into(),
    //                             background: false,
    //                             pipelines: vec![Pipeline {
    //                                 run_if: RunIf::Always,
    //                                 commands: vec![Command::If {
    //                                     condition: vec![Term {
    //                                         code: "sometimes-true".into(),
    //                                         pipelines: vec![Pipeline {
    //                                             run_if: RunIf::Always,
    //                                             commands: vec![Command::SimpleCommand {
    //                                                 external: false,
    //                                                 argv: vec![lit!("sometimes-true")],
    //                                                 redirects: vec![],
    //                                                 assignments: vec![],
    //                                             }],
    //                                         }],
    //                                         background: false,
    //                                     }],
    //                                     then_part: vec![Term {
    //                                         code: "break".into(),
    //                                         pipelines: vec![Pipeline {
    //                                             run_if: RunIf::Always,
    //                                             commands: vec![Command::Break],
    //                                         }],
    //                                         background: false,
    //                                     }],
    //                                     elif_parts: vec![],
    //                                     else_part: None,
    //                                     redirects: vec![],
    //                                 }]
    //                             }]
    //                         },
    //                         Term {
    //                             code: "if sometimes-true; then\n       continue;\n   fi".into(),
    //                             background: false,
    //                             pipelines: vec![Pipeline {
    //                                 run_if: RunIf::Always,
    //                                 commands: vec![Command::If {
    //                                     condition: vec![Term {
    //                                         code: "sometimes-true".into(),
    //                                         pipelines: vec![Pipeline {
    //                                             run_if: RunIf::Always,
    //                                             commands: vec![Command::SimpleCommand {
    //                                                 external: false,
    //                                                 argv: vec![lit!("sometimes-true")],
    //                                                 redirects: vec![],
    //                                                 assignments: vec![],
    //                                             }],
    //                                         }],
    //                                         background: false,
    //                                     }],
    //                                     then_part: vec![Term {
    //                                         code: "continue".into(),
    //                                         pipelines: vec![Pipeline {
    //                                             run_if: RunIf::Always,
    //                                             commands: vec![Command::Continue],
    //                                         }],
    //                                         background: false,
    //                                     }],
    //                                     elif_parts: vec![],
    //                                     else_part: None,
    //                                     redirects: vec![],
    //                                 }]
    //                             }]
    //                         },
    //                         Term {
    //                             code: "something".into(),
    //                             background: true,
    //                             pipelines: vec![Pipeline {
    //                                     run_if: RunIf::Always,
    //                                     commands: vec![Command::SimpleCommand {
    //                                         external: false,
    //                                         argv: vec![
    //                                             lit!("something"),
    //                                         ],
    //                                         redirects: vec![],
    //                                         assignments: vec![],
    //                                     }],
    //                                 }
    //                             ]
    //                         },
    //                     ],
    //                 }],
    //             }],
    //         }],
    //     })
    // );

    // assert_eq!(
    //     parse("{ echo hello; echo world; }"),
    //     Ok(Ast {
    //         terms: vec![Term {
    //             code: "{ echo hello; echo world; }".into(),
    //             background: false,
    //             pipelines: vec![Pipeline {
    //                 run_if: RunIf::Always,
    //                 commands: vec![Command::Group {
    //                     terms: vec![
    //                         Term {
    //                             code: "echo hello".into(),
    //                             background: false,
    //                             pipelines: vec![Pipeline {
    //                                 run_if: RunIf::Always,
    //                                 commands: vec![Command::SimpleCommand {
    //                                     external: false,
    //                                     argv: literal_word_vec!["echo", "hello"],
    //                                     redirects: vec![],
    //                                     assignments: vec![],
    //                                 }],
    //                             }],
    //                         },
    //                         Term {
    //                             code: "echo world".into(),
    //                             background: false,
    //                             pipelines: vec![Pipeline {
    //                                 run_if: RunIf::Always,
    //                                 commands: vec![Command::SimpleCommand {
    //                                     external: false,
    //                                     argv: literal_word_vec!["echo", "world"],
    //                                     redirects: vec![],
    //                                     assignments: vec![],
    //                                 }],
    //                             }],
    //                         },
    //                     ],
    //                 }],
    //             }],
    //         }],
    //     })
    // );

    // assert_eq!(
    //     parse("( echo hello; echo world; )"),
    //     Ok(Ast {
    //         terms: vec![Term {
    //             code: "( echo hello; echo world; )".into(),
    //             background: false,
    //             pipelines: vec![Pipeline {
    //                 run_if: RunIf::Always,
    //                 commands: vec![Command::SubShellGroup {
    //                     terms: vec![
    //                         Term {
    //                             code: "echo hello".into(),
    //                             background: false,
    //                             pipelines: vec![Pipeline {
    //                                 run_if: RunIf::Always,
    //                                 commands: vec![Command::SimpleCommand {
    //                                     external: false,
    //                                     argv: literal_word_vec!["echo", "hello"],
    //                                     redirects: vec![],
    //                                     assignments: vec![],
    //                                 }],
    //                             }],
    //                         },
    //                         Term {
    //                             code: "echo world".into(),
    //                             background: false,
    //                             pipelines: vec![Pipeline {
    //                                 run_if: RunIf::Always,
    //                                 commands: vec![Command::SimpleCommand {
    //                                     external: false,
    //                                     argv: literal_word_vec!["echo", "world"],
    //                                     redirects: vec![],
    //                                     assignments: vec![],
    //                                 }],
    //                             }],
    //                         },
    //                     ],
    //                 }],
    //             }],
    //         }],
    //     })
    // );

    // assert_eq!(
    //     parse(concat!(
    //         "case $action in\n",
    //         "echo) echo action is echo ;;\n",
    //         "date | time) echo action is date; date ;;\n",
    //         "esac"
    //     )),
    //     Ok(Ast {
    //         terms: vec![Term {
    //             code: "case $action in\necho) echo action is echo ;;\ndate | time) echo action is date; date ;;\nesac".into(),
    //             background: false,
    //             pipelines: vec![Pipeline {
    //                 run_if: RunIf::Always,
    //                 commands: vec![Command::Case {
    //                     word: param!("action", ExpansionOp::GetOrEmpty, false),
    //                     cases: vec![
    //                         CaseItem {
    //                             patterns: vec![lit!("echo")],
    //                             body: vec![Term {
    //                                 code: "echo action is echo".into(),
    //                                 background: false,
    //                                 pipelines: vec![Pipeline {
    //                                     run_if: RunIf::Always,
    //                                     commands: vec![Command::SimpleCommand {
    //                                         external: false,
    //                                         argv: literal_word_vec!["echo", "action", "is", "echo"],
    //                                         redirects: vec![],
    //                                         assignments: vec![],
    //                                     }],
    //                                 }],
    //                             }],
    //                         },
    //                         CaseItem {
    //                             patterns: vec![lit!("date"), lit!("time")],
    //                             body: vec![
    //                                 Term {
    //                                     code: "echo action is date".into(),
    //                                     background: false,
    //                                     pipelines: vec![Pipeline {
    //                                         run_if: RunIf::Always,
    //                                         commands: vec![Command::SimpleCommand {
    //                                             external: false,
    //                                             argv: literal_word_vec![
    //                                                 "echo", "action", "is", "date"
    //                                             ],
    //                                             redirects: vec![],
    //                                             assignments: vec![],
    //                                         }],
    //                                     }],
    //                                 },
    //                                 Term {
    //                                     code: "date".into(),
    //                                     background: false,
    //                                     pipelines: vec![Pipeline {
    //                                         run_if: RunIf::Always,
    //                                         commands: vec![Command::SimpleCommand {
    //                                             external: false,
    //                                             argv: literal_word_vec!["date"],
    //                                             redirects: vec![],
    //                                             assignments: vec![],
    //                                         }],
    //                                     }],
    //                                 },
    //                             ],
    //                         },
    //                     ],
    //                 }],
    //             }],
    //         }],
    //     })
    // );

    // assert_eq!(
    //     parse("function func1() { echo hello; echo world; return 3; }; func1"),
    //     Ok(Ast {
    //         terms: vec![
    //             Term {
    //                 code: "function func1() { echo hello; echo world; return 3; }".into(),
    //                 background: false,
    //                 pipelines: vec![Pipeline {
    //                     run_if: RunIf::Always,
    //                     commands: vec![Command::FunctionDef {
    //                         name: "func1".into(),
    //                         body: Box::new(Command::Group {
    //                             terms: vec![
    //                                 Term {
    //                                     code: "echo hello".into(),
    //                                     background: false,
    //                                     pipelines: vec![Pipeline {
    //                                         run_if: RunIf::Always,
    //                                         commands: vec![Command::SimpleCommand {
    //                                             external: false,
    //                                             argv: literal_word_vec!["echo", "hello"],
    //                                             redirects: vec![],
    //                                             assignments: vec![],
    //                                         }],
    //                                     }],
    //                                 },
    //                                 Term {
    //                                     code: "echo world".into(),
    //                                     background: false,
    //                                     pipelines: vec![Pipeline {
    //                                         run_if: RunIf::Always,
    //                                         commands: vec![Command::SimpleCommand {
    //                                             external: false,
    //                                             argv: literal_word_vec!["echo", "world"],
    //                                             redirects: vec![],
    //                                             assignments: vec![],
    //                                         }],
    //                                     }],
    //                                 },
    //                                 Term {
    //                                     code: "return 3".into(),
    //                                     background: false,
    //                                     pipelines: vec![Pipeline {
    //                                         run_if: RunIf::Always,
    //                                         commands: vec![Command::Return {
    //                                             status: Some(Word(vec![Span::Literal(3.to_string())]))
    //                                         }],
    //                                     }],
    //                                 },
    //                             ],
    //                         }),
    //                     }],
    //                 }],
    //             },
    //             Term {
    //                 code: "func1".into(),
    //                 background: false,
    //                 pipelines: vec![Pipeline {
    //                     run_if: RunIf::Always,
    //                     commands: vec![Command::SimpleCommand {
    //                         external: false,
    //                         argv: vec![lit!("func1")],
    //                         redirects: vec![],
    //                         assignments: vec![],
    //                     }],
    //                 }],
    //             },
    //         ],
    //     })
    // );

    // assert_eq!(
    //     parse("x=$((123)); func2() { local x=456 y z; echo $((x * 2))\n return; }; echo $x"),
    //     Ok(Ast {
    //         terms: vec![
    //             Term {
    //                 code: "x=$((123))".into(),
    //                 background: false,
    //                 pipelines: vec![Pipeline {
    //                     run_if: RunIf::Always,
    //                     commands: vec![Command::Assignment {
    //                         assignments: vec![Assignment {
    //                             append: false,
    //                             name: "x".into(),
    //                             initializer: Initializer::String(Word(vec![Span::ArithExpr {
    //                                 expr: Expr::Literal(123)
    //                             }])),
    //                             index: None,
    //                         }],
    //                     }],
    //                 }],
    //             },
    //             Term {
    //                 code: "func2() { local x=456 y z; echo $((x * 2))\n return; }".into(),
    //                 background: false,
    //                 pipelines: vec![Pipeline {
    //                     run_if: RunIf::Always,
    //                     commands: vec![Command::FunctionDef {
    //                         name: "func2".into(),
    //                         body: Box::new(Command::Group {
    //                             terms: vec![
    //                                 Term {
    //                                     code: "local x=456 y z".into(),
    //                                     background: false,
    //                                     pipelines: vec![Pipeline {
    //                                         run_if: RunIf::Always,
    //                                         commands: vec![Command::LocalDef {
    //                                             declarations: vec![
    //                                                 LocalDeclaration::Assignment(Assignment {
    //                                                     name: "x".into(),
    //                                                     initializer: Initializer::String(Word(
    //                                                         vec![Span::Literal("456".into())]
    //                                                     )),
    //                                                     index: None,
    //                                                     append: false,
    //                                                 }),
    //                                                 LocalDeclaration::Name("y".into()),
    //                                                 LocalDeclaration::Name("z".into())
    //                                             ]
    //                                         }],
    //                                     }],
    //                                 },
    //                                 Term {
    //                                     code: "echo $((x * 2))".into(),
    //                                     background: false,
    //                                     pipelines: vec![Pipeline {
    //                                         run_if: RunIf::Always,
    //                                         commands: vec![Command::SimpleCommand {
    //                                             external: false,
    //                                             argv: vec![
    //                                                 lit!("echo"),
    //                                                 Word(vec![Span::ArithExpr {
    //                                                     expr: Expr::Mul(BinaryExpr {
    //                                                         lhs: Box::new(Expr::Parameter {
    //                                                             name: "x".into()
    //                                                         }),
    //                                                         rhs: Box::new(Expr::Literal(2)),
    //                                                     })
    //                                                 }])
    //                                             ],
    //                                             redirects: vec![],
    //                                             assignments: vec![],
    //                                         }],
    //                                     }],
    //                                 },
    //                                 Term {
    //                                     code: "return".into(),
    //                                     background: false,
    //                                     pipelines: vec![Pipeline {
    //                                         run_if: RunIf::Always,
    //                                         commands: vec![Command::Return { status: None }],
    //                                     }],
    //                                 },
    //                             ],
    //                         }),
    //                     }],
    //                 }],
    //             },
    //             Term {
    //                 code: "echo $x".into(),
    //                 background: false,
    //                 pipelines: vec![Pipeline {
    //                     run_if: RunIf::Always,
    //                     commands: vec![Command::SimpleCommand {
    //                         external: false,
    //                         argv: vec![
    //                             lit!("echo"),
    //                             Word(vec![Span::Parameter {
    //                                 name: "x".into(),
    //                                 op: ExpansionOp::GetOrEmpty,
    //                                 quoted: false,
    //                             }])
    //                         ],
    //                         redirects: vec![],
    //                         assignments: vec![],
    //                     }],
    //                 }],
    //             },
    //         ],
    //     })
    // );

    // assert_eq!(
    //     parse("for ((i = 0; i < 4; i++));\ndo echo $i\ndone"),
    //     Ok(Ast {
    //         terms: vec![Term {
    //             code: "for ((i = 0; i < 4; i++));\ndo echo $i\ndone".into(),
    //             background: false,
    //             pipelines: vec![Pipeline {
    //                 run_if: RunIf::Always,
    //                 commands: vec![Command::ArithFor {
    //                     init: Expr::Assign {
    //                         name: "i".to_owned(),
    //                         rhs: Box::new(Expr::Literal(0))
    //                     },
    //                     cond: Expr::Lt(
    //                         Box::new(Expr::Parameter { name: "i".into() }),
    //                         Box::new(Expr::Literal(4))
    //                     ),
    //                     update: Expr::Inc("i".to_owned()),
    //                     body: vec![Term {
    //                         code: "echo $i".into(),
    //                         background: false,
    //                         pipelines: vec![Pipeline {
    //                             run_if: RunIf::Always,
    //                             commands: vec![Command::SimpleCommand {
    //                                 external: false,
    //                                 argv: vec![
    //                                     Word(vec![Span::Literal("echo".into())]),
    //                                     Word(vec![Span::Parameter {
    //                                         name: "i".into(),
    //                                         op: ExpansionOp::GetOrEmpty,
    //                                         quoted: false,
    //                                     }]),
    //                                 ],
    //                                 redirects: vec![],
    //                                 assignments: vec![],
    //                             }]
    //                         }],
    //                     }],
    //                 }],
    //             }],
    //         }],
    //     })
    // );
}

