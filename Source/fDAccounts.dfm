object DAccounts: TDAccounts
  Left = 0
  Top = 0
  HelpContext = 1065
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DAccounts'
  ClientHeight = 273
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    337
    273)
  PixelsPerInch = 106
  TextHeight = 13
  object GAccounts: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 322
    Height = 216
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'GAccounts'
    TabOrder = 0
    DesignSize = (
      322
      216)
    object FBNew: TButton
      Left = 8
      Top = 178
      Width = 93
      Height = 25
      Action = aNew
      Anchors = [akLeft, akBottom]
      TabOrder = 1
    end
    object FBDelete: TButton
      Left = 110
      Top = 178
      Width = 93
      Height = 25
      Action = aDelete
      Anchors = [akLeft, akBottom]
      TabOrder = 2
    end
    object FBEdit: TButton
      Left = 213
      Top = 178
      Width = 93
      Height = 25
      Action = aEdit
      Anchors = [akLeft, akBottom]
      TabOrder = 3
    end
    object PAccounts: TPanel_Ext
      Left = 8
      Top = 16
      Width = 306
      Height = 149
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvRaised
      BevelOuter = bvLowered
      ParentBackground = False
      TabOrder = 0
      object FAccounts: TListView_Ext
        Left = 2
        Top = 2
        Width = 302
        Height = 145
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Account'
            Width = 150
          end
          item
            Caption = 'LastLogin'
            Width = 150
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        PopupMenu = PopupMenu
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = FAccountsColumnClick
        OnCompare = FAccountsCompare
        OnDblClick = FAccountsDblClick
        OnResize = FAccountsResize
        OnSelectItem = FAccountsSelectItem
      end
    end
  end
  object FBOk: TButton
    Left = 166
    Top = 240
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object FBCancel: TButton
    Left = 254
    Top = 240
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 2
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 112
    Top = 224
    object miOpen: TMenuItem
      Action = aOpen
      Default = True
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miNew: TMenuItem
      Action = aNew
    end
    object miDelete: TMenuItem
      Action = aDelete
    end
    object miEdit: TMenuItem
      Action = aEdit
    end
  end
  object ActionList: TActionList
    Left = 56
    Top = 224
    object aNew: TAction
      Caption = 'aNew'
      ShortCut = 45
      OnExecute = aNewExecute
    end
    object aEdit: TAction
      Caption = 'aEdit'
      ShortCut = 32781
      OnExecute = aEditExecute
    end
    object aDelete: TAction
      Caption = 'aDelete'
      ShortCut = 46
      OnExecute = aDeleteExecute
    end
    object aOpen: TAction
      Caption = 'aOpen'
      OnExecute = aOpenExecute
    end
  end
end
