program ABC081A
    !s1,s2,s3：マス目s1~s3
    !cnt     ：ビー玉のあるマスの数
    implicit none
    integer s1, s2, s3
    integer cnt

    !入力
    read (*, '(*(i1))') s1, s2, s3

    !ビー玉のあるマスのカウント
    cnt = 0
    if (s1 == 1) cnt = cnt + 1
    if (s2 == 1) cnt = cnt + 1
    if (s3 == 1) cnt = cnt + 1

    !結果の出力
    write (*, '(i0)') cnt

end program ABC081A
