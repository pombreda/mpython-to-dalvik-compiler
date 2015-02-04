{funcsP =
  [{idF = "contador";
    paramsF =
     [("a",
       {tipagem = Some TInt; v_inicial = None; endereco = None;
        valor_variavel = None});
      ("b",
       {tipagem = Some TInt; v_inicial = None; endereco = None;
        valor_variavel = None})];
    cmdsF =
     [{vcmd =
        CmdAtrib
         ({valor = Some (ExpVar (VarSimples "soma")); tipo = None;
           pos =
            {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
             col_final = 0}},
         {valor = Some (ExpInt 0); tipo = None;
          pos =
           {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
            col_final = 0}});
       pcmd =
        {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
         col_final = 0}};
      {vcmd =
        CmdAtrib
         ({valor = Some (ExpVar (VarSimples "i")); tipo = None;
           pos =
            {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
             col_final = 0}},
         {valor = Some (ExpInt 0); tipo = None;
          pos =
           {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
            col_final = 0}});
       pcmd =
        {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
         col_final = 0}};
      {vcmd =
        CmdFor
         ({valor = Some (ExpVar (VarSimples "i")); tipo = None;
           pos =
            {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
             col_final = 0}},
         {vcmd = CmdRange (0, 5, 1);
          pcmd =
           {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
            col_final = 0}},
         [{vcmd =
            CmdAtrib
             ({valor = Some (ExpVar (VarSimples "soma")); tipo = None;
               pos =
                {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
                 col_final = 0}},
             {valor =
               Some
                (ExpBin (Mais,
                  {valor =
                    Some
                     (ExpBin (Mais,
                       {valor = Some (ExpVar (VarSimples "soma"));
                        tipo = None;
                        pos =
                         {Asa.Posicao.lin_inicial = 7; col_inicial = 1;
                          lin_final = 7; col_final = 0}},
                       {valor = Some (ExpVar (VarSimples "a")); tipo = None;
                        pos =
                         {Asa.Posicao.lin_inicial = 7; col_inicial = 1;
                          lin_final = 7; col_final = 0}}));
                   tipo = None;
                   pos =
                    {Asa.Posicao.lin_inicial = 7; col_inicial = 1;
                     lin_final = 7; col_final = 0}},
                  {valor = Some (ExpVar (VarSimples "b")); tipo = None;
                   pos =
                    {Asa.Posicao.lin_inicial = 7; col_inicial = 1;
                     lin_final = 7; col_final = 0}}));
              tipo = None;
              pos =
               {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
                col_final = 0}});
           pcmd =
            {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
             col_final = 0}}]);
       pcmd =
        {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
         col_final = 0}};
      {vcmd =
        CmdReturn
         {valor = Some (ExpVar (VarSimples "soma")); tipo = None;
          pos =
           {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
            col_final = 0}};
       pcmd =
        {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
         col_final = 0}}];
    returnF = Some TInt;
    posF =
     {Asa.Posicao.lin_inicial = 7; col_inicial = 1; lin_final = 7;
      col_final = 0};
    varLocaisF = <abstr>}];
 cmdsP = []}
