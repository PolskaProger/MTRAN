Imports System.Text.RegularExpressions
Imports System.Collections.Generic

' Класс, представляющий токен (также используется для ошибок)
Public Class Token
    Public Property Type As String        ' Тип токена или ошибки
    Public Property Value As String       ' Текст лексемы или сообщение об ошибке
    Public Property LexemeIndex As Integer ' Уникальный ID токена или ошибки

    Public Sub New(type As String, value As String, lexemeIndex As Integer)
        Me.Type = type
        Me.Value = value
        Me.LexemeIndex = lexemeIndex
    End Sub

    Public Overrides Function ToString() As String
        Return "<" & Type & ">(" & Value & ") [ID = " & LexemeIndex & "]"
    End Function
End Class

' Класс лексического анализатора
Public Class LexicalAnalyzer
    Private ReadOnly keywords As New List(Of String) From {
        "echo", "if", "else", "for", "while", "do", "switch", "function",
        "case", "break", "default", "foreach", "as", "return", "array"
    }

    Private ReadOnly tokenPatterns As New List(Of KeyValuePair(Of String, String)) From {
        New KeyValuePair(Of String, String)("WHITESPACE", "^\s+"),
        New KeyValuePair(Of String, String)("COMMENT", "^\/\/.*|^\/\*[\s\S]*?\*\/"),
        New KeyValuePair(Of String, String)("OPEN_TAG", "^\<\?php"),
        New KeyValuePair(Of String, String)("CLOSE_TAG", "^\?\>"),
        New KeyValuePair(Of String, String)("IDENTIFIER", "^\$?[a-zA-Z_]\w*"),
        New KeyValuePair(Of String, String)("NUMBER", "^\d+(\.\d+)?"),
        New KeyValuePair(Of String, String)("STRING", "^""([^""\\]*(\\.[^""\\]*)*)""|^'([^'\\]*(\\.[^'\\]*)*)'"),
        New KeyValuePair(Of String, String)("OPERATOR", "^(=>|==|!=|<=|>=|\+\+|\-\-|\+|\-|\*|\/|=|<|>|\.)"),
        New KeyValuePair(Of String, String)("BOOLEAN", "^(true|false)"),
        New KeyValuePair(Of String, String)("DELIMITER", "^[\(\)\{\}\[\],;:]")
    }

    Private currentUniqueID As Integer = 1
    Public Tokens As New List(Of Token)
    Public ErrorTokens As New List(Of Token)

    ' Анализатор возвращает список распознанных токенов, а ошибки сохраняет в ErrorTokens
    Public Function Analyze(ByVal input As String) As List(Of Token)
        Tokens.Clear()
        ErrorTokens.Clear()
        currentUniqueID = 1
        Dim position As Integer = 0
        Dim errorCount As Integer = 0

        While input.Length > 0
            ' --- Проверка 1: Неправильное имя переменной (например, "$1variable") ---
            If input.StartsWith("$") AndAlso input.Length > 1 AndAlso Char.IsDigit(input, 1) Then
                Dim mInvalid As Match = Regex.Match(input, "^\$\d\w*")
                Dim errorLexeme As String = mInvalid.Value
                Dim errorMsg As String = "Ошибка в позиции " & position & ": неправильное имя переменной '" & errorLexeme & "'. Переменная должна начинаться с '$' и буквы или '_'."
                Dim errorToken As New Token("INVALID_VARIABLE", errorMsg, currentUniqueID)
                currentUniqueID += 1
                ErrorTokens.Add(errorToken)
                errorCount += 1
                input = input.Substring(mInvalid.Length)
                position += mInvalid.Length
                Continue While
            End If

            ' --- Проверка 2: Незавершённый многострочный комментарий ---
            If input.StartsWith("/*") Then
                Dim endIndex As Integer = input.IndexOf("*/")
                If endIndex = -1 Then
                    Dim errorMsg As String = "Ошибка в позиции " & position & ": незавершённый многострочный комментарий."
                    Dim errorToken As New Token("UNFINISHED_COMMENT", errorMsg, currentUniqueID)
                    currentUniqueID += 1
                    ErrorTokens.Add(errorToken)
                    errorCount += 1
                    input = input.Substring(input.Length)
                    position += input.Length
                    Continue While
                End If
            End If

            ' --- Проверка 3: Незавершённый строковый литерал ---
            If input.StartsWith("""") Then
                Dim closingIndex As Integer = input.IndexOf("""", 1)
                If closingIndex = -1 Then
                    Dim errorMsg As String = "Ошибка в позиции " & position & ": незавершённый строковый литерал."
                    Dim errorToken As New Token("UNFINISHED_STRING", errorMsg, currentUniqueID)
                    currentUniqueID += 1
                    ErrorTokens.Add(errorToken)
                    errorCount += 1
                    input = input.Substring(input.Length)
                    position += input.Length
                    Continue While
                End If
            ElseIf input.StartsWith("'") Then
                Dim closingIndex As Integer = input.IndexOf("'", 1)
                If closingIndex = -1 Then
                    Dim errorMsg As String = "Ошибка в позиции " & position & ": незавершённый строковый литерал."
                    Dim errorToken As New Token("UNFINISHED_STRING", errorMsg, currentUniqueID)
                    currentUniqueID += 1
                    ErrorTokens.Add(errorToken)
                    errorCount += 1
                    input = input.Substring(input.Length)
                    position += input.Length
                    Continue While
                End If
            End If

            Dim matched As Boolean = False
            For Each kvp In tokenPatterns
                Dim pattern As String = kvp.Value
                Dim regex As New Regex(pattern)
                Dim m As Match = regex.Match(input)
                If m.Success Then
                    Dim lexeme As String = m.Value
                    If kvp.Key <> "WHITESPACE" AndAlso kvp.Key <> "COMMENT" Then
                        Dim tokenType As String = kvp.Key
                        ' Проверка на ключевое слово
                        If kvp.Key = "IDENTIFIER" AndAlso keywords.Contains(lexeme) Then
                            tokenType = "KEYWORD"
                        End If
                        Dim token As New Token(tokenType, lexeme, currentUniqueID)
                        currentUniqueID += 1
                        Tokens.Add(token)
                    End If
                    input = input.Substring(lexeme.Length)
                    position += lexeme.Length
                    matched = True
                    Exit For
                End If
            Next

            ' --- Проверка 4: Неопознанный символ ---
            If Not matched Then
                Dim errorLexeme As String = input.Substring(0, 1)
                Dim errorMsg As String = "Ошибка в позиции " & position & ": неопознанный символ '" & errorLexeme & "'."
                Dim errorToken As New Token("UNRECOGNIZED", errorMsg, currentUniqueID)
                currentUniqueID += 1
                ErrorTokens.Add(errorToken)
                errorCount += 1
                input = input.Substring(1)
                position += 1
            End If
        End While

        Return Tokens
    End Function
End Class