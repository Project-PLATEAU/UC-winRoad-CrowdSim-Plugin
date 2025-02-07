unit ExternalTrigger;

interface

uses
    SysUtils;

type
    ExternalTriggerClass = class
        private
            p_name : AnsiString;
        public
            constructor Create(const name: AnsiString);
            procedure   Fire();
            property    Name    : AnsiString read p_name;
        end;

implementation

uses
    MengeWrapper;

{ ExternalTriggerClass }

constructor ExternalTriggerClass.Create(const name: AnsiString);
    begin
    p_name := name;
    end;

procedure ExternalTriggerClass.Fire;
    var
        str: AnsiString;
    begin
    str := p_name;
//    MengeWrapper.FireExternalTrigger(PAnsiChar(@str[1]));
    end;

end.
