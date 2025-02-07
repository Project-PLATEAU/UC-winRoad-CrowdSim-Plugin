unit Vector3;

interface

type
    TVector3 = packed record
        private
            p_x: Single;
            p_y: Single;
            p_z: Single;
            procedure   SetX(const Value: Single);
            procedure   SetY(const Value: Single);
            procedure   SetZ(const Value: Single);
        public
            class operator Equal(a, b: TVector3): Boolean;
            class operator NotEqual(a, b: TVector3): Boolean;
            class operator Add(a, b: TVector3): TVector3;
            class operator Subtract(a, b: TVector3): TVector3;


            procedure   Initialize(x: Single = 0; y: Single = 0; z: Single = 0);
            procedure   CopyFrom(v: TVector3); overload;
            procedure   CopyFrom(x: Single = 0; y: Single = 0; z: Single = 0); overload;

            property    X: Single read p_x write SetX;
            property    Y: Single read p_y write SetY;
            property    Z: Single read p_z write SetZ;
        end;


implementation

{ TVector3 }

class operator TVector3.Add(a, b: TVector3): TVector3;
    begin
    result.Initialize(a.X + b.X, a.Y + b.Y, a.Z + b.Z);
    end;

procedure TVector3.CopyFrom(v: TVector3);
    begin
    p_x := v.X;
    p_y := v.Y;
    p_z := v.Z;
    end;

procedure TVector3.CopyFrom(x, y, z: Single);
    begin
    p_x := x;
    p_y := y;
    p_z := z;
    end;

class operator TVector3.Equal(a, b: TVector3): Boolean;
    begin
    result := ((a.X - b.X) < 1E-6) and ((a.Y - b.Y) < 1E-6) and ((a.Z - b.Z) < 1E-6);
    end;

procedure TVector3.Initialize(x, y, z: Single);
    begin
    p_x := x;
    p_y := y;
    p_z := z;
    end;

class operator TVector3.NotEqual(a, b: TVector3): Boolean;
    begin
    result := not (a = b);
    end;

procedure TVector3.SetX(const Value: Single);
    begin
    p_x := Value;
    end;

procedure TVector3.SetY(const Value: Single);
    begin
    p_y := Value;
    end;

procedure TVector3.SetZ(const Value: Single);
    begin
    p_z := Value;
    end;

class operator TVector3.Subtract(a, b: TVector3): TVector3;
    begin
    result.Initialize(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    end;

end.
