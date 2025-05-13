Imports System.Collections.Generic

Public Class Interpreter
    Private symbolTables As New Stack(Of Dictionary(Of String, Object))
    Private output As New List(Of String)

    Public Sub New()
        ' Инициализируем единственную таблицу символов
        symbolTables.Push(New Dictionary(Of String, Object)())
    End Sub

    Public Sub Interpret(syntaxTree As ASTNode)
        VisitNode(syntaxTree)
    End Sub

    Private Sub VisitNode(node As ASTNode)
        Select Case node.NodeType
            Case "Program", "Block", "Statement"
                For Each child In node.Children
                    VisitNode(child)
                Next
            Case "Assignment"
                VisitAssignment(node)
            Case "ExpressionStatement"
                VisitExpressionStatement(node)
            Case "ForStatement"
                VisitForStatement(node)
            Case "WhileStatement"
                VisitWhileStatement(node)
            Case "DoWhileStatement"
                VisitDoWhileStatement(node)
            Case "IfStatement"
                VisitIfStatement(node)
            Case "SwitchStatement"
                VisitSwitchStatement(node)
            Case "FunctionDeclaration"
                VisitFunctionDeclaration(node)
            Case "EchoStatement"
                VisitEchoStatement(node)
            Case "OPEN_TAG", "CLOSE_TAG"
                ' Игнорируем метки
            Case "BreakStatement"
                ' Обрабатывается в VisitSwitchStatement
            Case "BinaryExpression", "UnaryExpression"
                EvaluateExpression(node) ' Для вложенных выражений
            Case Else
                Throw New Exception("Неизвестный тип узла: " & node.NodeType)
        End Select
    End Sub

    Private Sub VisitAssignment(node As ASTNode)
        Dim identifier As String = node.Children(0).Value
        Dim value As Object = EvaluateExpression(node.Children(1))
        symbolTables.Peek()(identifier) = value
    End Sub

    Private Sub VisitExpressionStatement(node As ASTNode)
        EvaluateExpression(node.Children(0))
    End Sub

    Private Sub VisitForStatement(node As ASTNode)
        Dim initNode As ASTNode = node.Children(0).Children(0)
        If initNode.NodeType <> "Empty" Then VisitNode(initNode)

        Dim conditionNode As ASTNode = node.Children(1).Children(0)
        Dim incrementNode As ASTNode = node.Children(2).Children(0)
        Dim bodyNode As ASTNode = node.Children(3).Children(0)

        While True
            Dim condVal As Boolean = If(conditionNode.NodeType = "Empty", True, CBool(EvaluateExpression(conditionNode)))
            If Not condVal Then Exit While
            VisitNode(bodyNode)
            If incrementNode.NodeType <> "Empty" Then VisitNode(incrementNode)
        End While
    End Sub

    Private Sub VisitWhileStatement(node As ASTNode)
        Dim condNode As ASTNode = node.Children(0).Children(0)
        Dim bodyNode As ASTNode = node.Children(1).Children(0)
        While CBool(EvaluateExpression(condNode))
            VisitNode(bodyNode)
        End While
    End Sub

    Private Sub VisitDoWhileStatement(node As ASTNode)
        Dim bodyNode As ASTNode = node.Children(0).Children(0)
        Dim condNode As ASTNode = node.Children(1).Children(0)
        Do
            VisitNode(bodyNode)
        Loop While CBool(EvaluateExpression(condNode))
    End Sub

    Private Sub VisitIfStatement(node As ASTNode)
        Dim condNode As ASTNode = node.Children(0).Children(0)
        Dim thenNode As ASTNode = node.Children(1).Children(0)
        Dim elseNode As ASTNode = If(node.Children.Count > 2, node.Children(2).Children(0), Nothing)
        If CBool(EvaluateExpression(condNode)) Then
            VisitNode(thenNode)
        ElseIf elseNode IsNot Nothing Then
            VisitNode(elseNode)
        End If
    End Sub

    Private Sub VisitSwitchStatement(node As ASTNode)
        Dim exprNode As ASTNode = node.Children(0).Children(0)
        Dim exprValue As Object = EvaluateExpression(exprNode)
        Dim breakFlag As Boolean = False
        For i As Integer = 1 To node.Children.Count - 1
            If breakFlag Then Exit For
            Dim caseNode As ASTNode = node.Children(i)
            If caseNode.NodeType = "Case" Then
                Dim caseVal As Object = EvaluateExpression(caseNode.Children(0))
                If Equals(exprValue, caseVal) Then
                    For j As Integer = 1 To caseNode.Children.Count - 1
                        Dim stmt = caseNode.Children(j)
                        If stmt.NodeType = "BreakStatement" Then breakFlag = True : Exit For
                        VisitNode(stmt)
                    Next
                    Exit For
                End If
            ElseIf caseNode.NodeType = "Default" Then
                For Each stmt In caseNode.Children
                    If stmt.NodeType = "BreakStatement" Then breakFlag = True : Exit For
                    VisitNode(stmt)
                Next
                Exit For
            End If
        Next
    End Sub

    Private Sub VisitFunctionDeclaration(node As ASTNode)
        ' Игнорируем
    End Sub

    Private Sub VisitEchoStatement(node As ASTNode)
        For Each arg In node.Children(0).Children
            Dim val As Object = EvaluateExpression(arg)
            output.Add(If(val Is Nothing, String.Empty, val.ToString()))
        Next
    End Sub

    Private Function EvaluateExpression(expr As ASTNode) As Object
        Select Case expr.NodeType
            Case "Literal"
                Dim txt = expr.Value
                If IsNumeric(txt) Then Return If(txt.Contains("."), Double.Parse(txt), Integer.Parse(txt))
                If txt.Equals("true", StringComparison.OrdinalIgnoreCase) Then Return True
                If txt.Equals("false", StringComparison.OrdinalIgnoreCase) Then Return False
                Return txt

            Case "Identifier"
                Dim id = expr.Value
                For Each tbl In symbolTables
                    If tbl.ContainsKey(id) Then Return tbl(id)
                Next
                symbolTables.Peek()(id) = Nothing
                Return Nothing

            Case "BinaryExpression"
                Dim left = EvaluateExpression(expr.Children(0))
                Dim right = EvaluateExpression(expr.Children(1))
                Dim op = expr.Value
                Select Case op
                    Case "."
                        Return If(left?.ToString(), String.Empty) & If(right?.ToString(), String.Empty)
                    Case Else
                        Dim lnum As Double = If(left Is Nothing, 0, CDbl(left))
                        Dim rnum As Double = If(right Is Nothing, 0, CDbl(right))
                        Select Case op
                            Case "+" : Return lnum + rnum
                            Case "-" : Return lnum - rnum
                            Case "*" : Return lnum * rnum
                            Case "/" : Return lnum / rnum
                            Case "==" : Return Equals(left, right)
                            Case "!=" : Return Not Equals(left, right)
                            Case "<" : Return lnum < rnum
                            Case ">" : Return lnum > rnum
                            Case "<=" : Return lnum <= rnum
                            Case ">=" : Return lnum >= rnum
                            Case "=" : Return right
                            Case Else : Throw New Exception("Неизвестный оператор: " & op)
                        End Select
                End Select

            Case "UnaryExpression"
                Dim op = expr.Value
                Dim child = expr.Children(0)
                Dim orig = EvaluateExpression(child)
                Dim val As Double = If(orig Is Nothing, 0, CDbl(orig))
                Select Case op
                    Case "++"
                        val += 1
                        If child.NodeType = "Identifier" Then symbolTables.Peek()(child.Value) = val
                        Return val
                    Case "--"
                        val -= 1
                        If child.NodeType = "Identifier" Then symbolTables.Peek()(child.Value) = val
                        Return val
                    Case Else : Throw New Exception("Неизвестный унарный оператор: " & op)
                End Select

            Case Else
                Throw New Exception("Неизвестный тип выражения: " & expr.NodeType)
        End Select
    End Function

    Public Function GetOutput() As String
        Return String.Join(Environment.NewLine, output)
    End Function
End Class
