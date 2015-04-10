Public Function ADC_FastLinearity(return_data() As Long, Site As Long, sample_size As Long, Maj As Long, Mid As Long, Tail As Long, Lsb As Long, flag As Integer, VM As Double, VP As Double, vrefL As Double, vrefH As Double, bits As Long) As Long
    Dim ind() As Long
    Dim x() As Double
    Dim y() As Double
    Dim yc() As Double
    Dim i As Long, j As Long, k As Long
    Dim a As Double, m As Double, b As Double, l As Double
    Dim ye() As Double
    Dim yTail() As Double
    Dim yMid() As Double
    Dim yMaj() As Double
    Dim nn() As Double
    Dim N As Long
'    Dim Tail As Long, Mid As Long, Maj As Long
    Dim yref() As Double
    Dim yTail_ref() As Double
    Dim yMid_ref() As Double
    Dim yMaj_ref() As Double
    Dim eMaj() As Double
    Dim cnt As Long
    Dim INL() As Double
    Dim eMid() As Double
    Dim eTail() As Double
'    Dim flag As Integer
    Dim DNL() As Double
    Dim meanDNL As Double
'    Dim bits As Long
    Dim maxindex As Long
    Dim textline As String
    Dim CapArrayFile As String
    Dim actual_site As Long
    
    Dim MeanHits As Double
    Dim hist() As Double
    Dim dnlLSB() As Double
    Dim inlLSB() As Double
    Dim hits As Long
    Dim OffsetError As Double
    Dim FullScaleError As Double
    Dim OffsetError_V As Double
    Dim FullScaleError_V As Double
    Dim GainError As Double
    Dim idealLSB As Double ' Value in volt
    Dim HitCounter() As Double
    Dim S As Double
    Dim Tg1 As Double
    Dim TgN As Double
    Dim T_ideal1 As Double
    Dim T_idealN As Double
    Dim AvgHits As Double
    
'                TheHdw.StartStopwatch
    On Error GoTo errhandler
    hits = 4
'    CapArrayFile = "C:\MSP430\Dongyu\dirMSP430FR4133_20140627\prog\Readout\ADC10RampData ADC_5M_AVCC_mod_50ksps_sht2_prod_30_0_2014_09_19_17_09_39.ad1"
    
'    Open CapArrayFile For Input As #1
    
'    maxindex = 0
'    Erase yc
'
'    'Read txt file to generate information array
'    While Not EOF(1)
'        ReDim Preserve yc(maxindex)
'        Line Input #1, textline
'        yc(maxindex) = CLng(textline)
'        maxindex = maxindex + 1
'    Wend
    
'    Close #1

    actual_site = Site
    N = 2 ^ bits - 1
    ReDim yc(sample_size)
    
'''    For i = 0 To sample_size
'''        yc(i) = return_data(i, Site)
'''    Next i
    ReDim HitCounter(N)
    j = 0
    For i = 0 To sample_size
        If (i >= 0 And i < 100) Or (i >= 582 And i < 666) Or (i >= 1158 And i < 1242) Or (i >= 1734 And i < 1818) Or (i >= 2822 And i < 2906) Or (i >= 3398 And i < 3482) Or (i >= 4477 And i < 4577) Then
            HitCounter(return_data(i, Site)) = 1 / 4 + HitCounter(return_data(i, Site))
            If (i >= 0 And i < 100) Then
                If (i Mod 4 = 0) Then
                    yc(j) = return_data(i, Site)
                    j = j + 1
                End If
            ElseIf (i >= 4477 And i < 4577) Then
                If (i - 4477) Mod 4 = 0 Then
                    yc(j) = return_data(i, Site)
                    j = j + 1
                End If
            Else
                If (i - 582) Mod 4 = 0 Then
                    yc(j) = CLng((return_data(i, Site) + return_data(i + 1, Site) + return_data(i + 2, Site) + return_data(i + 3, Site)) / 4)
                    j = j + 1
                End If
            End If
        Else
            HitCounter(return_data(i, Site)) = 1 + HitCounter(return_data(i, Site))
            yc(j) = return_data(i, Site)
            j = j + 1
        End If
    Next i
    ReDim Preserve yc(j - 1)
    
    idealLSB = (vrefH - vrefL) / N
    
    T_ideal1 = vrefL + 0.5 * idealLSB
    T_idealN = vrefL + (N - 0.5) * idealLSB
    

' Start Calculate histogram cumulate
'----------------------------------------------
'' VM = negative peak of the ramp
'' VP = positive peak of the ramp
    S = UBound(yc) + 1 'Hc(N) * 1#
'' Tg(1) is transistion voltage at the lower edge of the "1" code
'' Hits(at first trans)/Hits(total) = V(range to first trans)/V(full range)
''       Hc(0)         /      S     =     Tg(1)-VM           /    (VP-VM)
    Tg1 = VM + (VP - VM) / S * HitCounter(0)
'' Tg(N) is the transistion voltage at the lower edge of the "n-1" code
    TgN = VM + (VP - VM) / S * (S - HitCounter(N))
' ################################################
' start calculate Offest / FullSCale / Gain Error
    OffsetError = (Tg1 - T_ideal1) / idealLSB
    OffsetError_V = OffsetError * idealLSB
    FullScaleError = (TgN - T_idealN) / idealLSB
    FullScaleError_V = FullScaleError * idealLSB
    GainError = FullScaleError - OffsetError
' stop calculate Offest / FullSCale / Gain Error
    AvgHits = (S - HitCounter(N) - HitCounter(0)) / (N - 1)

    If AvgHits = 0 Then AvgHits = 0.01




    For i = 0 To UBound(yc)
        If yc(i) > yc(0) Then Exit For
    Next i
    
    For j = UBound(yc) To 0 Step -1
        If yc(j) < yc(UBound(yc)) Then Exit For
    Next j
    ReDim x(j - i)
    ReDim y(j - i)
    ReDim ye(j - i)
    ReDim yTail(j - i)
    ReDim yMid(j - i)
    ReDim yMaj(j - i)
    
    For k = i To j
        x(k - i) = k
        y(k - i) = yc(k)
    Next k
    
    ReDim yc(UBound(y))
    For i = 0 To UBound(y)
        yc(i) = y(i)
    Next

    If Lsb > 0 Then
        ReDim hist(2 ^ Lsb - 1)
        ReDim dnlLSB(2 ^ Lsb - 1)
        ReDim inlLSB(2 ^ Lsb - 1)
    
    
        For i = LBound(x) To UBound(x)
            hist(y(i) Mod (2 ^ Lsb)) = hist(y(i) Mod (2 ^ Lsb)) + 1
        Next i
            
        MeanHits = 0
        For i = 0 To 2 ^ Lsb - 1
            MeanHits = MeanHits + hist(i)
        Next
        MeanHits = MeanHits / (2 ^ Lsb)
        For i = 0 To 2 ^ Lsb - 1
            dnlLSB(i) = hist(i) / MeanHits - 1
            If i = 0 Then
                inlLSB(i) = 0
            Else
                inlLSB(i) = inlLSB(i - 1) + dnlLSB(i - 1)
            End If
        Next i
        
    ''    ReDim yc(UBound(y))
    ''    For i = 0 To UBound(y)
    ''        yc(i) = y(i)
    ''    Next
    
        For i = 0 To UBound(y)
            y(i) = y(i) + inlLSB(y(i) Mod (2 ^ Lsb)) + dnlLSB(y(i) Mod (2 ^ Lsb)) / 2
        Next i
    End If
''    For i = 0 To UBound(y)
''        For k = 0 To 2 ^ Lsb - 1
''            If y(i) Mod (2 ^ Lsb) = k Then
''                y(i) = y(i) + inlLSB(k) + dnlLSB(k) / 2
''                Exit For
''            End If
''        Next k
''    Next i

    'solve for LS fit line
    Call BestFit(x(), y(), a, m)

    'deviation from LS fit line
    For i = LBound(x) To UBound(x)
        ye(i) = a * x(i) + m - y(i)
            yTail(i) = yc(i) Mod (2 ^ Tail)
            yMid(i) = yc(i) Mod (2 ^ (Mid + Tail)) - yTail(i)
            yMaj(i) = yc(i) - yMid(i) - yTail(i)
            'redefine yTail with the tail+2
'            yTail(i) = yc(i) Mod (2 ^ (Tail + 2))
    Next i
    ReDim nn(N)
    ReDim yref(N)
    ReDim yTail_ref(N)
    ReDim yMid_ref(N)
    ReDim yMaj_ref(N)
    ReDim INL(N)
    ReDim DNL(N)
    For i = 0 To N
        nn(i) = i
        yref(i) = i
        yTail_ref(i) = yref(i) Mod (2 ^ Tail)
        yMid_ref(i) = yref(i) Mod (2 ^ (Mid + Tail)) - yTail_ref(i)
        yMaj_ref(i) = yref(i) - yMid_ref(i) - yTail_ref(i)
        'redefine yTail_ref with the tail+2
'        yTail_ref(i) = yref(i) Mod (2 ^ (Tail + 2))
    Next i
    
    Dim o As Long, p As Long

    '%% compute center errors for each MSB segment
    ReDim eMaj(2 ^ Maj - 1)
    For k = 0 To (2 ^ Maj) - 1
        eMaj(k) = 0
        cnt = 0
        ReDim ind(0)
        o = MaxofTwo(LBound(x), (k - 0.5) * (2 ^ (Mid + Tail)) * hits)
        p = MinofTwo(UBound(x), (k + 1.5) * (2 ^ (Mid + Tail)) * hits)
        For i = o To p
            If yMaj(i) = k * (2 ^ (Mid + Tail)) + yMaj(0) Then
                j = UBound(ind)
                ind(j) = i
                ReDim Preserve ind(j + 1)
                
                eMaj(k) = eMaj(k) + ye(i)
                cnt = cnt + 1
            End If
        Next i
        If cnt > 0 Then
            eMaj(k) = eMaj(k) / cnt
            For i = LBound(ind) To UBound(ind) - 1
                ye(ind(i)) = ye(ind(i)) - eMaj(k)
            Next i
        End If
        For i = k * (2 ^ (Mid + Tail)) To (k + 1) * (2 ^ (Mid + Tail)) - 1 'For i = 0 To N
            If yMaj_ref(i) = k * (2 ^ (Mid + Tail)) Then
                INL(i) = eMaj(k)
            End If
        Next i
    Next k

    '%% compute center errors for each Mid segment
    If Mid > 0 Then
        ReDim eMid(2 ^ Mid - 1)
        For k = 0 To (2 ^ Mid) - 1
            eMid(k) = 0
            cnt = 0
            ReDim ind(0)
            For i = LBound(x) To UBound(x)
                If yMid(i) = k * (2 ^ Tail) Then
                    j = UBound(ind)
                    ind(j) = i
                    ReDim Preserve ind(j + 1)
                    
                    eMid(k) = eMid(k) + ye(i)
                    cnt = cnt + 1
                End If
            Next i
            If cnt > 0 Then
                eMid(k) = eMid(k) / cnt
                For i = LBound(ind) To UBound(ind) - 1
                    ye(ind(i)) = ye(ind(i)) - eMid(k)
                Next i
            End If
            For i = 0 To N
                If yMid_ref(i) = k * (2 ^ Tail) Then
                    INL(i) = INL(i) + eMid(k)
                End If
            Next i
        Next k
    End If
    
    'increase the tail to tail+2
'    Tail = Tail + 2
    '%% compute center errors for each Tail segment
    ReDim eTail(2 ^ Tail - 1)
    For k = 0 To (2 ^ Tail) - 1
        eTail(k) = 0
        cnt = 0
        ReDim ind(0)
        For i = 0 To UBound(x) 'For i = LBound(x) To UBound(x)
            If yTail(i) = k Then
                j = UBound(ind)
                ind(j) = i
                ReDim Preserve ind(j + 1)
                
                eTail(k) = eTail(k) + ye(i)
                cnt = cnt + 1
            End If
        Next i
        If cnt > 0 Then
            eTail(k) = eTail(k) / cnt
            For i = LBound(ind) To UBound(ind) - 1
                ye(ind(i)) = ye(ind(i)) - eTail(k)
            Next i
        End If
        For i = k To N + 1 - 2 ^ Tail + k Step (2 ^ Tail)
            If yTail_ref(i) = k Then
                INL(i) = INL(i) + eTail(k)
            End If
        Next i
    Next k
    
    If Lsb > 0 Then
        '%move from ideal LSB code bin center back to actual
        For i = 0 To N
            INL(i) = INL(i) + inlLSB(i Mod (2 ^ Lsb)) + dnlLSB(i Mod (2 ^ Lsb)) / 2
        Next i
''    For i = 0 To N
''        For k = 0 To 2 ^ Lsb - 1
''            If i Mod (2 ^ Lsb) = k Then
''                INL(i) = INL(i) + inlLSB(k) + dnlLSB(k) / 2
''                Exit For
''            End If
''        Next k
''    Next i
    End If
    
    If flag = 0 Then
    '%% Compute end-point or LS best fit line INL DNL
        For i = 0 To N - 1
            DNL(i) = INL(i + 1) - INL(i)
            meanDNL = meanDNL + DNL(i)
        Next i
        meanDNL = meanDNL / N
        For i = 0 To N
            If i = N Then
                DNL(i) = 0
            Else
                DNL(i) = DNL(i) - meanDNL
            End If
            If i = 0 Then
                INL(i) = 0
            Else
                INL(i) = INL(i - 1) + DNL(i - 1)
            End If
        Next i
    
    ElseIf flag = 1 Then 'Best fit line INL DNL
        Call BestFit(nn, INL, b, l)
        For i = 0 To N
            INL(i) = INL(i) - (b * nn(i) + l)
            If i > 0 Then
                DNL(i - 1) = INL(i) - INL(i - 1)
            End If
        Next i
        DNL(N) = 0
    End If
    '%% account for missing codes
    For i = 0 To N
        If DNL(i) < -1 Then
            If i = 0 Then
                DNL(N) = DNL(N) + DNL(i) + 1
            Else
                DNL(i - 1) = DNL(i - 1) + DNL(i) + 1
            End If
            DNL(i) = -1
        End If
    Next i
'    DNL(767) = (DNL(767) + HitCounter(767) / AvgHits - 1) / 2
'    DNL(639) = (DNL(639) + HitCounter(639) / AvgHits - 1) / 2
'    DNL(255) = (DNL(255) + HitCounter(255) / AvgHits - 1) / 2
'    DNL(127) = (DNL(127) + HitCounter(127) / AvgHits - 1) / 2

    Dim MaxINLg As Double, MinINLg As Double, MaxDNLg As Double, MinDNLg As Double
    Dim PosMaxINL As Long, PosMinINL As Long, PosMaxDNL As Long, PosMinDNL As Long
    Dim ll As Double, LD As Double
    MaxINLg = -999
    MinINLg = 999
    MaxDNLg = -999
    MinDNLg = 999
    ll = 0
    For i = 1 To N - 1
        If INL(i) > MaxINLg Then
            MaxINLg = INL(i)
            PosMaxINL = i
        End If
        If INL(i) < MinINLg Then
            MinINLg = INL(i)
            PosMinINL = i
        End If
        If DNL(i) > MaxDNLg Then
            MaxDNLg = DNL(i)
            PosMaxDNL = i
        End If
        If DNL(i) < MinDNLg Then
            MinDNLg = DNL(i)
            PosMinDNL = i
        End If
        If DNL(i) = -1 Then
            ll = ll + 1
        End If
    Next i
    Dim Spec_INL As Double, Spec_DNL As Double
' calculate Spec_INL
    If Abs(MaxINLg) > Abs(MinINLg) Then
       Spec_INL = Abs(MaxINLg)
    Else
       Spec_INL = Abs(MinINLg)
    End If
    If Abs(MaxDNLg) > Abs(MinDNLg) Then
       Spec_DNL = Abs(MaxDNLg)
    Else
       Spec_DNL = Abs(MinDNLg)
    End If
    
    Dim teststatus As Long
    Dim tnum As Long
    Dim forcecond As Double
    Dim u_spec As Double
    Dim l_spec As Double
    
    forcecond = 3
' Start datalog for missing codes
            If ll <= 10# Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            LD = ll * 1#
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "missing codes", 0, -0.1, LD, _
            10, 0, forcecond, unitVolt, 0)
' Stop datalog for missing codes

' Start datalog for spec_inl
            u_spec = CDbl(DC_Spec_Val("ADC_INL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_INL_min"))
            If Spec_INL > l_spec And Spec_INL < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "INL", 0, l_spec, Spec_INL, _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for spec_inl
 
' Start datalog for max_inl
            u_spec = CDbl(DC_Spec_Val("ADC_INL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_INL_min"))
            If MaxINLg > l_spec And MaxINLg < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "Max INL", 0, l_spec, MaxINLg, _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for max_inl
            
' Start datalog for min_inl
            u_spec = CDbl(DC_Spec_Val("ADC_INL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_INL_min"))
            If MinINLg > l_spec And MinINLg < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "Min INL", 0, l_spec, MinINLg, _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for min_inl
            
' Start datalog for spec_dnl
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If Spec_DNL > l_spec And Spec_DNL < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL", 0, l_spec, Spec_DNL, _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for spec_dnl
 
' Start datalog for max_DNL
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If MaxDNLg > l_spec And MaxDNLg < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "Max DNL", 0, l_spec, MaxDNLg, _
            u_spec, unitLSB, forcecond, unitVolt, 0)
    ' Stop datalog for max_DNL
            
' Start datalog for min_DNL
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If MinDNLg > l_spec And MinDNLg < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "Min DNL", 0, l_spec, MinDNLg, _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for min_DNL
            'global variable for TLV Structure
            ADC_OFFSET_LSB(Site) = OffsetError
'            ADC_OFFSET_LSB(site) = Not ADC_OFFSET_LSB(site)
            ' Fix 16 bit issue
            'If ADC_OFFSET_LSB(Site) < -1 Then ADC_OFFSET_LSB(Site) = ADC_OFFSET_LSB(Site) + 65536
            ' Add 1 for 2nd complement
            'ADC_OFFSET_LSB(Site) = ADC_OFFSET_LSB(Site) + 1

            u_spec = CDbl(DC_Spec_Val("ADC_offset_max")) / idealLSB / 1000
            l_spec = CDbl(DC_Spec_Val("ADC_offset_min")) / idealLSB / 1000
            If OffsetError > l_spec And OffsetError < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "Offset Error", 0, l_spec, OffsetError, _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for Offset Error
            
' Start datalog for Offset Error in volt

            u_spec = u_spec * idealLSB
            l_spec = l_spec * idealLSB
            If OffsetError_V > l_spec And OffsetError_V < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "Offset Error V", 0, l_spec, OffsetError_V, _
            u_spec, unitVolt, forcecond, unitVolt, 0)
' Stop datalog for Offset Error in volt
            
' Start datalog for Full Scale Error (FFF) = FSE
            u_spec = CDbl(DC_Spec_Val("ADC_FSE_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_FSE_min"))
            If FullScaleError > -10# And FullScaleError < 10# Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "FSE Error", 0, -10#, FullScaleError, _
            10#, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for Full Scale Error (FFF) = FSE
            
' Start datalog for Full Scale Error in V (FFF) = FSE
            u_spec = 1#
            l_spec = -1#
            If FullScaleError_V > -1# And FullScaleError_V < 1# Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "FSE Error V", 0, -10#, FullScaleError_V, _
            10#, unitVolt, forcecond, unitVolt, 0)
' Stop datalog for Full Scale Error in V(FFF) = FSE

' Start datalog for Gain Error
            'global variable for TLV Structure
            'ATE gain errror definition is different from the app definition.
            'So the calculation formula is corrected - Dongyu Ou
            ADC_GAIN_FACTOR(actual_site) = 32768 / (1023 / (1023 + GainError))
            'How to caluculate as of meeting from 10.2.2010
            'GAIN_ERROR = FULLSCALE_ERROR - OFFSET_ERROR
            'Gain = (1023 + GAIN_ERROR) / 1023
            'GAIN_FACTOR = 1 / Gain * 32768
            
            u_spec = CDbl(DC_Spec_Val("ADC_gain_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_gain_min"))
            If GainError > l_spec And GainError < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "Gain Error", 0, l_spec, GainError, _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for Gain Error

' Start datalog for position for max DNL
            If PosMaxDNL > 0 And PosMaxDNL < 1024 Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "PosMaxDNL", 0, 0#, CDbl(PosMaxDNL), _
            1024#, unitNone, forcecond, unitVolt, 0)
' Stop datalog for position for max DNL

' Start datalog for position for min DNL
            If PosMinDNL > 0 And PosMinDNL < 1024 Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "PosMinDNL", 0, 0#, CDbl(PosMinDNL), _
            1024#, unitNone, forcecond, unitVolt, 0)
' Stop datalog for position for max DNL

' Start datalog for position for max INL
            If PosMaxINL > 0 And PosMaxINL < 1024 Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "PosMaxiNL", 0, 0#, CDbl(PosMaxDNL), _
            1024#, unitNone, forcecond, unitVolt, 0)
' Stop datalog for position for max INL

' Start datalog for position for min INL
            If PosMinINL > 0 And PosMinINL < 1024 Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "PosMinINL", 0, 0#, CDbl(PosMinINL), _
            1024#, unitNone, forcecond, unitVolt, 0)
' Stop datalog for position for min INL
' Start datalog for DNL at position 127
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(127) > l_spec And DNL(127) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_127", 0, l_spec, DNL(127), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 127
 
' Start datalog for DNL at position 128
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(128) > l_spec And DNL(128) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_128", 0, l_spec, DNL(128), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 128

' Start datalog for DNL at position 255
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(255) > l_spec And DNL(255) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_255", 0, l_spec, DNL(255), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 255
 
' Start datalog for DNL at position 256
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(256) > l_spec And DNL(256) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_256", 0, l_spec, DNL(256), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 256
 
' Start datalog for DNL at position 383
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(383) > l_spec And DNL(383) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_383", 0, l_spec, DNL(383), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 383
 
' Start datalog for DNL at position 384
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(384) > l_spec And DNL(384) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_384", 0, l_spec, DNL(384), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 384

' Start datalog for DNL at position 511
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(511) > l_spec And DNL(511) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_511", 0, l_spec, DNL(511), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 511
 
' Start datalog for DNL at position 512
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(512) > l_spec And DNL(512) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_512", 0, l_spec, DNL(512), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 512
 
' Start datalog for DNL at position 639
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(639) > l_spec And DNL(639) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_639", 0, l_spec, DNL(639), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 639
 
' Start datalog for DNL at position 640
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(640) > l_spec And DNL(640) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_640", 0, l_spec, DNL(640), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 640
 
' Start datalog for DNL at position 767
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(767) > l_spec And DNL(767) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_767", 0, l_spec, DNL(767), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 767
 
' Start datalog for DNL at position 768
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(768) > l_spec And DNL(768) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_768", 0, l_spec, DNL(768), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 768
 
' Start datalog for DNL at position 895
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(895) > l_spec And DNL(895) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_895", 0, l_spec, DNL(895), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 895
 
' Start datalog for DNL at position 896
            u_spec = CDbl(DC_Spec_Val("ADC_DNL_max"))
            l_spec = CDbl(DC_Spec_Val("ADC_DNL_min"))
            If DNL(896) > l_spec And DNL(896) < u_spec Then
                teststatus = logTestPass
                TheExec.sites.Site(actual_site).TestResult = SitePass
            Else
                teststatus = logTestFail
                TheExec.sites.Site(actual_site).TestResult = sitefail
            End If
            tnum = TheExec.sites.Site(actual_site).TestNumber
            Call TheExec.Datalog.WriteParametricresult _
            (actual_site, tnum, teststatus, 0, "DNL_896", 0, l_spec, DNL(896), _
            u_spec, unitLSB, forcecond, unitVolt, 0)
' Stop datalog for DNL at position 896
''    Dim TimeStamp As String, instanceName As String
''        TimeStamp = format(Date, "yyyy_mm_dd_") & format(Time, "hh_mm_ss")
''
''        instanceName = TheExec.DataManager.instanceName
''        Open TheBook.path & "\Readout\ADC10DNL " & instanceName & "_" & Site & "_" & TimeStamp & ".txt" For Append Access Write As #1
''        Close #1
''        Kill TheBook.path & "\Readout\ADC10DNL " & instanceName & "_" & Site & "_" & TimeStamp & ".txt"
''        Open TheBook.path & "\Readout\ADC10DNL " & instanceName & "_" & Site & "_" & TimeStamp & ".txt" For Append Access Write As #1
''            For i = 0 To N
''                Print #1, DNL(i)
''            Next i
''        Close
''
''        Open TheBook.path & "\Readout\ADC10INL " & instanceName & "_" & Site & "_" & TimeStamp & ".txt" For Append Access Write As #1
''        Close #1
''        Kill TheBook.path & "\Readout\ADC10INL " & instanceName & "_" & Site & "_" & TimeStamp & ".txt"
''        Open TheBook.path & "\Readout\ADC10INL " & instanceName & "_" & Site & "_" & TimeStamp & ".txt" For Append Access Write As #1
''            For i = 0 To N
''                Print #1, INL(i)
''            Next i
''        Close

Call Essential_DSP_Double(DNL, 0, N, "DNL2")
Call Essential_DSP_Double(INL, 0, N, "INL2")
    'Debug.Print TheHdw.ReadStopwatch
    Exit Function
errhandler:

End Function


Public Sub BestFit(x() As Double, y() As Double, ByRef Slope As Double, ByRef Intercept As Double)
' compute least squares regression best fit
' An Introduction to Mixed-Signal IC Test and Measurement
' Mark Burns and Gordon W. Roberts
' Oxford University Press 2001
' ISBN 0-19-514016-8
' Section 11.2, pg 407

' X and Y are arrays containing the X,Y pair data points of empirical data to fit

    Dim K1 As Double
    Dim K2 As Double
    Dim K3 As Double
    Dim K4 As Double
    Dim AVOID_ZERO_DIVION As Double
    AVOID_ZERO_DIVION = 1000
    
    K1 = 0#
    K2 = 0#
    K3 = 0#
    K4 = 0#
    
    Dim i As Long
    Dim N As Long
    N = UBound(x) - LBound(x) + 1
    
    For i = LBound(x) To UBound(x)
        K1 = K1 + x(i)
        K2 = K2 + y(i)
        K3 = K3 + (x(i) * x(i))
        K4 = K4 + (x(i) * y(i))
    Next

    ' tersw00192206 ->
    If ((N * K3) - (K1 * K1)) <> 0 Then
        Slope = ((N * K4) - (K1 * K2)) / ((N * K3) - (K1 * K1))
        Intercept = (K2 / N) - (Slope * K1 / N)
    Else
        Slope = AVOID_ZERO_DIVION
        Intercept = AVOID_ZERO_DIVION
    End If
    ' <- tersw00192206

End Sub

Public Function MaxofTwo(a As Long, b As Long) As Long
    If a >= b Then
        MaxofTwo = a
    Else
        MaxofTwo = b
    End If
End Function

Public Function MinofTwo(a As Long, b As Long) As Long
    If a <= b Then
        MinofTwo = a
    Else
        MinofTwo = b
    End If
End Function
