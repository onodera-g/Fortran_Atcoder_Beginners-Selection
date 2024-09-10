program ABC086C
    !N        ：目的地の数
    !dt       ：目的地間での経過時間
    !t_prev   ：現在時刻
    !x_prev   ：現在のx座標
    !y_prev   ：現在のy座標
    !t_curr   ：目的地のx座標
    !x_curr   ：目的地の到着時刻
    !y_curr   ：目的地のy座標
    !dt       ：現在時刻と到着時刻の差
    !dist     ：現在位置から目的地までの道のり
    !flag_move：目的地までの移動可否
    implicit none
    integer i, N
    integer t_prev, x_prev, y_prev, t_curr, x_curr, y_curr, dt, dist
    logical flag_move

    ! N 入力
    read (*, *) N
    t_prev = 0; x_prev = 0; y_prev = 0
    flag_move = .true.
    do i = 1, n
        !t,x,y 入力
        read (*, *) t_curr, x_curr, y_curr
        dt = t_curr - t_prev
        dist = abs(x_curr - x_prev) + abs(y_curr - y_prev)
        !移動判定
        if (dist > dt .or. mod(dist, 2) /= mod(dt, 2)) then
            flag_move = .false.
            exit
        end if
        !座標と時刻の更新
        t_prev = t_curr; x_prev = x_curr; y_prev = y_curr
    end do

    !結果の出力
    if (flag_move) then
        print *, 'Yes'
    else
        print *, 'No'
    end if
end program ABC086C
