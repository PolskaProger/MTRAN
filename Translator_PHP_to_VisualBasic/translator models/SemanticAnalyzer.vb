Imports System.Collections.Generic

Public Enum DataType
    IntegerType
    DoubleType
    StringType
    BooleanType
    ArrayType
    ObjectType
    Unknown
End Enum

Public Class Symbol
    Public Property Name As String
    Public Property Type As DataType

    Public Sub New(name As String, type As DataType)
        Me.Name = name
        Me.Type = type
    End Sub
End Class

Public Class SymbolTable
    Private symbols As New Dictionary(Of String, Symbol)

    Public Sub AddSymbol(name As String, type As DataType)
        If symbols.ContainsKey(name) Then
            Throw New Exception("Переменная " & name & " уже объявлена в этой области видимости.")
        End If
        symbols.Add(name, New Symbol(name, type))
    End Sub

    Public Function GetSymbol(name As String) As Symbol
        If symbols.ContainsKey(name) Then
            Return symbols(name)
        End If
        Return Nothing
    End Function
End Class

Public Class SemanticAnalyzer
    Private symbolTables As New Stack(Of SymbolTable)
    Private currentTable As SymbolTable
    Private errors As New List(Of String)

    Public Sub New()
        currentTable = New SymbolTable()
        symbolTables.Push(currentTable)
    End Sub

    ''' <summary>
    ''' Анализирует AST и возвращает список семантических ошибок.
    ''' </summary>
    Public Function Analyze(syntaxTree As ASTNode) As List(Of String)
        errors.Clear()
        AnalyzeNode(syntaxTree)
        Return errors
    End Function

    Private Sub ReportError(message As String)
        errors.Add(message)
    End Sub

    Private Sub AnalyzeNode(node As ASTNode)
        If node Is Nothing Then Return

        Select Case node.NodeType
            Case "Program", "Block", "Statement"
                For Each ch In node.Children
                    AnalyzeNode(ch)
                Next

            Case "Assignment"
                Try
                    AnalyzeAssignment(node)
                Catch ex As Exception
                    ReportError(ex.Message)
                End Try

            Case "BinaryExpression"
                ' Спец-обработка присваивания в заголовке for
                If node.Value = "=" AndAlso node.Children(0).NodeType = "Identifier" Then
                    Try
                        Dim id = node.Children(0).Value
                        Dim expr = node.Children(1)
                        Dim t = GetExpressionType(expr)
                        Dim sym = GetSymbol(id)
                        If sym Is Nothing Then
                            currentTable.AddSymbol(id, t)
                        ElseIf sym.Type <> t Then
                            ReportError($"Несоответствие типов при присваивании {id}. Ожидался {sym.Type}, получен {t}.")
                        End If
                    Catch ex As Exception
                        ReportError(ex.Message)
                    End Try
                Else
                    GetExpressionType(node)
                End If

            Case "ExpressionStatement"
                GetExpressionType(node.Children(0))

            Case "ForStatement"
                ' Инициализация, условие, инкремент, тело
                AnalyzeNode(node.Children(0).Children(0))
                AnalyzeNode(node.Children(1).Children(0))
                AnalyzeNode(node.Children(2).Children(0))
                AnalyzeNode(node.Children(3).Children(0))

            Case "WhileStatement"
                AnalyzeNode(node.Children(0).Children(0))
                AnalyzeNode(node.Children(1).Children(0))

            Case "DoWhileStatement"
                AnalyzeNode(node.Children(0).Children(0))
                AnalyzeNode(node.Children(1).Children(0))

            Case "IfStatement"
                AnalyzeNode(node.Children(0).Children(0))
                AnalyzeNode(node.Children(1).Children(0))
                If node.Children.Count > 2 Then AnalyzeNode(node.Children(2).Children(0))

            Case "SwitchStatement"
                AnalyzeNode(node.Children(0).Children(0))
                For i As Integer = 1 To node.Children.Count - 1
                    For Each ch In node.Children(i).Children
                        AnalyzeNode(ch)
                    Next
                Next

            Case "FunctionDeclaration"
                ' Новый scope для функции
                Dim fnTable = New SymbolTable()
                symbolTables.Push(fnTable)
                currentTable = fnTable
                For Each p In node.Children(1).Children
                    currentTable.AddSymbol(p.Value, DataType.Unknown)
                Next
                AnalyzeNode(node.Children(2).Children(0))
                symbolTables.Pop()
                currentTable = symbolTables.Peek()

            Case "EchoStatement"
                For Each arg In node.Children(0).Children
                    GetExpressionType(arg)
                Next

            Case "UnaryExpression"
                GetExpressionType(node)
        End Select
    End Sub

    Private Sub AnalyzeAssignment(node As ASTNode)
        Dim id = node.Children(0).Value
        Dim exprType = GetExpressionType(node.Children(1))
        Dim sym = GetSymbol(id)
        If sym Is Nothing Then
            currentTable.AddSymbol(id, exprType)
        ElseIf sym.Type <> exprType Then
            ReportError($"Несоответствие типов при присваивании {id}. Ожидался {sym.Type}, получен {exprType}.")
        End If
    End Sub

    Private Function GetExpressionType(expr As ASTNode) As DataType
        Select Case expr.NodeType
            Case "Literal"
                If IsNumeric(expr.Value) Then
                    Return If(expr.Value.Contains("."), DataType.DoubleType, DataType.IntegerType)
                ElseIf expr.Value.ToLower() = "true" OrElse expr.Value.ToLower() = "false" Then
                    Return DataType.BooleanType
                Else
                    Return DataType.StringType
                End If

            Case "Identifier"
                Dim s = GetSymbol(expr.Value)
                If s Is Nothing Then
                    ReportError($"Необъявленная переменная: {expr.Value}")
                    Return DataType.Unknown
                End If
                Return s.Type

            Case "BinaryExpression"
                Dim lt = GetExpressionType(expr.Children(0))
                Dim rt = GetExpressionType(expr.Children(1))
                Select Case expr.Value
                    Case "+", "-", "*", "/"
                        If lt = rt AndAlso lt = DataType.IntegerType Then Return DataType.IntegerType
                        If (lt = DataType.IntegerType OrElse lt = DataType.DoubleType) AndAlso
                           (rt = DataType.IntegerType OrElse rt = DataType.DoubleType) Then
                            Return DataType.DoubleType
                        End If
                        ReportError($"Недопустимые типы для оператора {expr.Value}")
                        Return DataType.Unknown
                    Case "==", "!=", "<", ">", "<=", ">="
                        If lt = rt Then Return DataType.BooleanType
                        ReportError("Несоответствие типов в сравнении")
                        Return DataType.Unknown
                End Select
                Return DataType.Unknown

            Case "UnaryExpression"
                Dim ot = GetExpressionType(expr.Children(0))
                If expr.Value = "++" OrElse expr.Value = "--" Then
                    If ot = DataType.IntegerType OrElse ot = DataType.DoubleType Then
                        Return ot
                    End If
                    ReportError($"Недопустимый тип для унарного оператора {expr.Value}")
                End If
                Return DataType.Unknown

            Case Else
                Return DataType.Unknown
        End Select
    End Function

    Private Function GetSymbol(name As String) As Symbol
        For Each tbl In symbolTables
            Dim s = tbl.GetSymbol(name)
            If s IsNot Nothing Then Return s
        Next
        Return Nothing
    End Function
End Class
