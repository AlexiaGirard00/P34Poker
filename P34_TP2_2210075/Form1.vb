Public Class Form1
    Dim ResourceSet As Resources.ResourceSet = My.Resources.ResourceManager.GetResourceSet(Globalization.CultureInfo.CurrentCulture, True, True)

    Dim listImages As New List(Of Image)
    Dim listNamesImages As New List(Of String)
    Dim selectedCards(4) As String

    Dim listPair As New List(Of Integer)
    Dim pairCount As Integer

    Dim gainTotal As Integer
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        gbCartes.Enabled = False
        txtGain.ReadOnly = True

        numMise.Maximum = 10

        For Each Dict As DictionaryEntry In ResourceSet.OfType(Of Object)
            If TypeOf Dict.Value Is Image Then
                listImages.Add(Dict.Value)
                listNamesImages.Add(Dict.Key)
            End If
        Next

        MessageBox.Show("Veuillez entrer le nombre de crédit puis appuyer sur jouer." & vbCrLf & vbCrLf & "Bonne chance !", "Poker")

    End Sub

    Private Sub btnJouer_Click(sender As Object, e As EventArgs) Handles btnJouer.Click

        If numCredit.Value <> 0 Then
            btnJouer.Enabled = True
            If numMise.Value <> 0 Then
                numCredit.Enabled = False
                gbCartes.Enabled = True
                btnJouer.Enabled = False

                StartingHand()
            Else
                MessageBox.Show("Vous devez miser au minimum 1", "Mise insuffisante")
            End If
        Else
            MessageBox.Show("Vous devez entrer au minimum 1 crédit.", "Crédit insuffisant")
        End If

    End Sub

    Private Sub btnContinuer_Click(sender As Object, e As EventArgs) Handles btnContinuer.Click

        numMise.Enabled = True

        SecondHand(pb1, chkCarte1, 0)
        SecondHand(pb2, chkCarte2, 1)
        SecondHand(pb3, chkCarte3, 2)
        SecondHand(pb4, chkCarte4, 3)
        SecondHand(pb5, chkCarte5, 4)

        Dim result As DialogResult
        Dim resultCredit As DialogResult
        Dim inputCredit As String = ""
        Dim valide = False

        If numMise.Value <> 0 Then
            btnContinuer.Enabled = False
            result = MessageBox.Show(CheckHands(selectedCards), "Resultat", MessageBoxButtons.YesNo)
            If result = DialogResult.Yes Then
                If numCredit.Value <> 0 Then
                    StartingHand()
                    For Each ctrl As CheckBox In gbCartes.Controls.OfType(Of CheckBox)
                        ctrl.Checked = False
                    Next
                    btnContinuer.Enabled = True
                Else
                    resultCredit = MessageBox.Show("Vous n'avez plus de crédit." & vbCrLf & "Désirez-vous en ajouter ?", "Crédit insuffisant", MessageBoxButtons.YesNo)
                    If resultCredit = DialogResult.Yes Then
                        Do
                            inputCredit = InputBox("Entrer le nombre de crédit.")
                            If IsNumeric(inputCredit) And inputCredit <> 0 Then
                                valide = True
                                numCredit.Value += CInt(inputCredit)
                                StartingHand()
                                btnContinuer.Enabled = True
                            Else
                                MessageBox.Show("Vous devez entrer un nombre supérieur à 0.", "Invalide")
                            End If
                        Loop While Not valide
                    Else
                        QuitterJeu()
                    End If
                End If
            Else
                QuitterJeu()
            End If
        Else
            MessageBox.Show("La mise minimum est de 1.", "Mise minimum")
        End If

    End Sub

    Private Sub btnQuitter_Click(sender As Object, e As EventArgs) Handles btnQuitter.Click
        Dim result As DialogResult
        result = MessageBox.Show("Désirez-vous vraiment quitter le jeu ?", "Quitter ?", MessageBoxButtons.YesNo)

        If result = DialogResult.Yes Then
            QuitterJeu()
        End If

    End Sub

    Private Sub QuitterJeu()
        MessageBox.Show("Voici vos gains: " & gainTotal & ".", "Aurevoir !")
        Close()
    End Sub

    Private Function RndIndex() As Integer
        Dim rndNumber As New Random
        Dim i As Integer = rndNumber.Next(0, listImages.Count)
        Return i

    End Function

    Private Function CheckBoxeCheck() As Boolean
        Dim count As Integer

        For Each Control In Me.Controls
            If TypeOf Control Is CheckBox Then
                If CType(Control, CheckBox).Checked Then
                    count += 1
                End If
            End If
        Next

        If count > 0 Then
            Return True
        Else
            Return False
        End If
    End Function

    Private Function CheckHands(cards() As String)
        Dim handResult As String
        Dim suit(4) As String
        Dim face(4) As String
        Dim intFace(4) As Integer

        Dim royal As Boolean = False

        Dim fullHouseValide As Boolean = False
        Dim fullHouseList As New List(Of Integer)

        Dim flushCount As Integer = 0
        Dim straightCount As Integer = 0

        Dim mise As Integer = numMise.Value

        Dim gain As Integer = 0

        'Séparer les couleurs et les valeurs

        'couleur
        For i As Integer = 0 To selectedCards.Length - 1
            suit(i) = selectedCards(i)
            face(i) = selectedCards(i)
        Next

        For i As Integer = 0 To suit.Length - 1
            suit(i) = suit(i).Substring(1, 1)
        Next

        'Valeur & enlever la parti suit
        For i As Integer = 0 To face.Length - 1
            Select Case face(i).Substring(0, 1)
                Case "t"
                    face(i) = "10"
                Case "j"
                    face(i) = "11"
                Case "q"
                    face(i) = "12"
                Case "k"
                    face(i) = "13"
                Case "a"
                    face(i) = "14"
                Case Else
                    face(i) = face(i).Substring(0, 1)
            End Select
        Next

        'remplir face()

        For i As Integer = 0 To face.Length - 1
            intFace(i) = face(i)
        Next

        Array.Sort(intFace)


        If intFace(0) = 2 And intFace(4) = 14 Then
            intFace(4) = 1
        ElseIf intFace(4) = 14 Then
            royal = True
        End If

        Array.Sort(intFace)

        'Check flush
        For i As Integer = 0 To suit.Length - 2
            If suit(i) = suit(i + 1) Then
                flushCount += 1
            End If
        Next

        'Check straight
        For i As Integer = 0 To intFace.Length - 2
            If intFace(i) + 1 = intFace(i + 1) Then
                straightCount += 1
            End If
        Next

        'check pair
        listPair.Clear()
        pairCount = 0

        CheckPair(face, 0)
        CheckPair(face, 1)
        CheckPair(face, 2)

        If face(3) = face(4) Then
            listPair.Add(face(3))
            pairCount += 1
        End If

        'Check Full house
        For i As Integer = 0 To face.Length - 1
            If listPair.Contains(face(i)) Then
            Else
                fullHouseList.Add(face(i))
            End If
        Next

        If fullHouseList.Count = 0 Then
            fullHouseValide = True
        End If


        If flushCount = 4 And straightCount = 4 And royal Then
            gain = mise * 100
            handResult = "Royal flush ! Vous avez fait un gain de " & gain & "." & vbCrLf & "Désirez vous continuer à jouer ?"
        ElseIf flushCount = 4 And straightCount = 4 Then
            gain = mise * 50
            handResult = "Straight flush ! Vous avez fait un gain de " & gain & "." & vbCrLf & "Désirez vous continuer à jouer ?"
        ElseIf straightCount = 4 Then
            gain = mise * 10
            handResult = "Straight ! Vous avez fait un gain de " & gain & "." & vbCrLf & "Désirez vous continuer à jouer ?"
        ElseIf flushCount = 4 Then
            gain = mise * 15
            handResult = "Flush ! Vous avez fait un gain de " & gain & "." & vbCrLf & "Désirez vous continuer à jouer ?"
        ElseIf pairCount = 6 Then
            gain = mise * 25
            handResult = "Four of kind! Vous avez fait un gain de " & gain & "." & vbCrLf & "Désirez vous continuer à jouer ?"
        ElseIf pairCount = 4 And fullHouseValide Then
            gain = mise * 20
            handResult = "Full house ! Vous avez fait un gain de " & gain & "." & vbCrLf & "Désirez vous continuer à jouer ?"
        ElseIf pairCount = 3 Then
            gain = mise * 5
            handResult = ("Three of kind! Vous avez fait un gain de " & gain) & "." & vbCrLf & "Désirez vous continuer à jouer ?"
        ElseIf pairCount = 1 Then
            gain = mise * 1
            handResult = ("Pair ! Vous avez fait un gain de " & gain) & "." & vbCrLf & "Désirez vous continuer à jouer ?"
        ElseIf pairCount = 2 Then
            gain = mise * 3
            handResult = ("Two pair ! Vous avez fait un gain de " & gain) & "." & vbCrLf & "Désirez vous continuer à jouer ?"
        Else
            handResult = ("Aucun gain") & "." & vbCrLf & "Désirez-vous continuer à jouer ?"
        End If

        gainTotal += gain
        txtGain.Text = gainTotal.ToString()
        numCredit.Value += gain

        Return handResult
    End Function

    Private Sub StartingHand()
        Dim i As Integer

        numCredit.Value -= 1

        i = RndIndex()
        pb1.Image = listImages(i)
        pb1.Tag = listNamesImages(i)

        i = RndIndex()
        pb2.Image = listImages(i)
        pb2.Tag = listNamesImages(i)

        i = RndIndex()
        pb3.Image = listImages(i)
        pb3.Tag = listNamesImages(i)

        i = RndIndex()
        pb4.Image = listImages(i)
        pb4.Tag = listNamesImages(i)

        i = RndIndex()
        pb5.Image = listImages(i)
        pb5.Tag = listNamesImages(i)
    End Sub

    Private Sub SecondHand(pb As PictureBox, chkCarte As CheckBox, j As Integer)
        Dim i As Integer

        If Not chkCarte.Checked Then
            i = RndIndex()
            pb.Image = listImages(i)
            pb.Tag = listNamesImages(i)
        End If

        selectedCards(j) = (pb.Tag).ToString.Substring(0, 2)
    End Sub

    Private Sub CheckPair(face() As String, j As Integer)

        For i As Integer = j To face.Length - 2
            If face(j) = face(i + 1) Then
                listPair.Add(face(j))
                pairCount += 1
            End If
        Next
    End Sub
End Class
