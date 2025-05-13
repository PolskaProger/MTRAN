Imports System
Imports System.Collections.Generic

Module Program
    Sub Main()
        Dim inputFile As String =
            If(Environment.GetCommandLineArgs().Length > 1,
               Environment.GetCommandLineArgs()(1),
               "INPUT.TXT")

        If Not IO.File.Exists(inputFile) Then
            Console.WriteLine("Файл " & inputFile & " не найден.")
            Return
        End If

        Dim code As String = IO.File.ReadAllText(inputFile)

        ' --- Лексический анализ ---
        Dim lexer As New LexicalAnalyzer()
        Dim tokens As List(Of Token) = lexer.Analyze(code)
        Console.WriteLine("Лексический анализ завершён." & vbCrLf)

        Console.WriteLine("Список токенов (без группировки):")
        For Each tk In tokens
            Console.WriteLine(tk.ToString())
        Next

        Dim tokensByType As New Dictionary(Of String, List(Of Token))
        For Each tk In tokens
            If Not tokensByType.ContainsKey(tk.Type) Then
                tokensByType.Add(tk.Type, New List(Of Token))
            End If
            tokensByType(tk.Type).Add(tk)
        Next

        For Each ttype In tokensByType.Keys
            Console.WriteLine(vbCrLf & "Таблица токенов типа " & ttype & ":")
            PrintUniqueTokenTable(tokensByType(ttype))
        Next

        Console.WriteLine(vbCrLf & "Таблица ошибок лексера:")
        PrintErrorTable(lexer.ErrorTokens)

        ' --- Синтаксический анализ ---
        Dim parser As New SyntacticAnalyzer()
        Dim syntaxTree As ASTNode = parser.Analyze(tokens)
        Console.WriteLine(vbCrLf & "Синтаксический анализ завершён.")

        ' Собираем синтаксические ошибки
        Dim syntaxErrors As List(Of String) = parser.SyntaxErrors
        Console.WriteLine("Синтаксические ошибки:")
        For Each msg In syntaxErrors
            Console.WriteLine("- " & msg)
        Next

        Console.WriteLine(vbCrLf & "Дерево разбора:")
        PrintSyntaxTree(syntaxTree, 0)

        ' --- Семантический анализ (только если синтаксических ошибок нет) ---
        Dim semanticErrors As New List(Of String)
        If syntaxErrors.Count = 0 Then
            Dim semanticAnalyzer As New SemanticAnalyzer()
            semanticErrors = semanticAnalyzer.Analyze(syntaxTree)
            Console.WriteLine(vbCrLf & "Семантические ошибки:")
            For Each msg In semanticErrors
                Console.WriteLine("- " & msg)
            Next
        End If

        Console.WriteLine(vbCrLf & "Итоги:")
        If syntaxErrors.Count > 0 Then
            Console.WriteLine("Программа не может быть выполнена из-за синтаксических ошибок.")
        ElseIf semanticErrors.Count > 0 Then
            Console.WriteLine("Программа не может быть выполнена из-за семантических ошибок.")
        Else
            ' --- Интерпретация ---
            Dim interpreter As New Interpreter()
            interpreter.Interpret(syntaxTree)
            Console.WriteLine(vbCrLf & "Результат выполнения программы:")
            Console.WriteLine(interpreter.GetOutput())
        End If

        Console.WriteLine(vbCrLf & "Нажмите любую клавишу для завершения...")
        Console.ReadKey()
    End Sub

    Sub PrintSyntaxTree(node As ASTNode, indent As Integer)
        Console.WriteLine(New String(" "c, indent * 4) & node.NodeType & If(node.Value <> "", ": " & node.Value, ""))
        For Each child In node.Children
            PrintSyntaxTree(child, indent + 1)
        Next
    End Sub

    Sub PrintUniqueTokenTable(tokens As List(Of Token))
        If tokens.Count = 0 Then
            Console.WriteLine("Нет токенов для отображения.")
            Return
        End If

        Dim colNoWidth As Integer = "№".Length
        Dim colLexemeWidth As Integer = "Лексема".Length
        Dim colIDsWidth As Integer = "ID".Length

        Dim unique As New Dictionary(Of String, List(Of Integer))
        For Each tk In tokens
            If Not unique.ContainsKey(tk.Value) Then unique.Add(tk.Value, New List(Of Integer))
            unique(tk.Value).Add(tk.LexemeIndex)
        Next

        For Each kvp In unique
            colLexemeWidth = Math.Max(colLexemeWidth, kvp.Key.Length)
            colIDsWidth = Math.Max(colIDsWidth, String.Join(", ", kvp.Value).Length)
        Next

        Dim sep = "+" & New String("-"c, colNoWidth + 2) &
                  "+" & New String("-"c, colLexemeWidth + 2) &
                  "+" & New String("-"c, colIDsWidth + 2) & "+"

        Console.WriteLine(sep)
        Console.WriteLine("| " & "№".PadRight(colNoWidth) & " | " &
                          "Лексема".PadRight(colLexemeWidth) & " | " &
                          "ID".PadRight(colIDsWidth) & " |")
        Console.WriteLine(sep)

        Dim i As Integer = 1
        For Each kvp In unique
            Dim ids = String.Join(", ", kvp.Value)
            Console.WriteLine("| " & i.ToString().PadRight(colNoWidth) & " | " &
                              kvp.Key.PadRight(colLexemeWidth) & " | " &
                              ids.PadRight(colIDsWidth) & " |")
            i += 1
        Next
        Console.WriteLine(sep)
    End Sub

    Sub PrintErrorTable(errorTokens As List(Of Token))
        If errorTokens.Count = 0 Then
            Console.WriteLine("Ошибок не обнаружено.")
            Return
        End If

        Dim colNoWidth As Integer = "№".Length
        Dim colTypeWidth As Integer = "Тип".Length
        Dim colMsgWidth As Integer = "Сообщение".Length

        For Each et In errorTokens
            colTypeWidth = Math.Max(colTypeWidth, et.Type.Length)
            colMsgWidth = Math.Max(colMsgWidth, et.Value.Length)
        Next

        Dim sep = "+" & New String("-"c, colNoWidth + 2) &
                  "+" & New String("-"c, colTypeWidth + 2) &
                  "+" & New String("-"c, colMsgWidth + 2) & "+"

        Console.WriteLine(sep)
        Console.WriteLine("| " & "№".PadRight(colNoWidth) & " | " &
                          "Тип".PadRight(colTypeWidth) & " | " &
                          "Сообщение".PadRight(colMsgWidth) & " |")
        Console.WriteLine(sep)

        Dim i As Integer = 1
        For Each et In errorTokens
            Console.WriteLine("| " & i.ToString().PadRight(colNoWidth) & " | " &
                              et.Type.PadRight(colTypeWidth) & " | " &
                              et.Value.PadRight(colMsgWidth) & " |")
            i += 1
        Next
        Console.WriteLine(sep)
    End Sub
End Module
