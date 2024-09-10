program ABC087B
    !A  ：500円玉の個数
    !B  ：100円玉の個数
    !C  ：50円玉の個数
    !X  ：合計金額
    !cnt：合計金額をX円にできる組み合わせの個数
    implicit none
    integer i, j, k
    integer A, B, C, X, cnt

    !入力
    read (*, *) A
    read (*, *) B
    read (*, *) C
    read (*, *) X

    !効果の組み合わせの検証
    cnt = 0
    do i = 0, A
        do j = 0, B
            do k = 0, C
                if (500*i + 100*j + 50*k == X) cnt = cnt + 1
            end do
        end do
    end do

    !結果の出力
    write (*, *) cnt

end program ABC087B
