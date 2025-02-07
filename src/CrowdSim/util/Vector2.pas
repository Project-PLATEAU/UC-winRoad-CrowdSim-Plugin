unit Vector2;

interface

type
    TVector2 = packed record
        private
            p_x: Single;
            p_y: Single;
            procedure   SetX(const Value: Single);
            procedure   SetY(const Value: Single);
        public
            class operator Equal(a, b: TVector2): Boolean;
            class operator NotEqual(a, b: TVector2): Boolean;
            class operator Add(a, b: TVector2): TVector2;
            class operator Subtract(a, b: TVector2): TVector2;


            procedure   Initialize(x: Single = 0; y: Single = 0);
            procedure   CopyFrom(x: Single; y: Single); overload;

            property    X: Single read p_x write SetX;
            property    Y: Single read p_y write SetY;
        end;

implementation


{ TVector2 }

class operator TVector2.Add(a, b: TVector2): TVector2;
    begin
    result.Initialize(a.X + b.X, a.Y + b.Y);
    end;

procedure TVector2.CopyFrom(x, y: Single);
    begin
    p_x := x;
    p_y := y;
    end;

class operator TVector2.Equal(a, b: TVector2): Boolean;
    begin
    result := ((a.X - b.X) < 1E-6) and ((a.Y - b.Y) < 1E-6);
    end;

procedure TVector2.Initialize(x, y: Single);
    begin
    p_x := x;
    p_y := y;
    end;

class operator TVector2.NotEqual(a, b: TVector2): Boolean;
    begin
    result := not (a = b);
    end;

procedure TVector2.SetX(const Value: Single);
    begin
    p_x := Value;
    end;

procedure TVector2.SetY(const Value: Single);
    begin
    p_y := Value;
    end;

class operator TVector2.Subtract(a, b: TVector2): TVector2;
    begin
    result.Initialize(a.X - b.X, a.Y - b.Y);
    end;

end.
