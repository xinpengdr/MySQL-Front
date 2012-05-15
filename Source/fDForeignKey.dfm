object DForeignKey: TDForeignKey
  Left = 436
  Top = 175
  BorderStyle = bsDialog
  Caption = 'DForeignKey'
  ClientHeight = 476
  ClientWidth = 404
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnHide = FormHide
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    404
    476)
  PixelsPerInch = 106
  TextHeight = 13
  object FBOk: TButton
    Left = 234
    Top = 443
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object FBCancel: TButton
    Left = 322
    Top = 443
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GBasics: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 389
    Height = 277
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'GBasics'
    TabOrder = 3
    DesignSize = (
      389
      277)
    object FLTable: TLabel
      Left = 8
      Top = 111
      Width = 39
      Height = 13
      Caption = 'FLTable'
      FocusControl = FParentTable
    end
    object FLFields: TLabel
      Left = 8
      Top = 143
      Width = 39
      Height = 13
      Caption = 'FLFields'
      FocusControl = FParentFields
    end
    object FLChild: TLabel
      Left = 120
      Top = 56
      Width = 35
      Height = 13
      Caption = 'FLChild'
    end
    object FLParent: TLabel
      Left = 256
      Top = 56
      Width = 43
      Height = 13
      Caption = 'FLParent'
    end
    object FLName: TLabel
      Left = 8
      Top = 19
      Width = 40
      Height = 13
      Caption = 'FLName'
      FocusControl = FName
    end
    object FLDatabase: TLabel
      Left = 8
      Top = 79
      Width = 58
      Height = 13
      Caption = 'FLDatabase'
      FocusControl = FParentDatabase
    end
    object FParentTable: TComboBox_Ext
      Left = 256
      Top = 108
      Width = 121
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnChange = FParentTableChange
    end
    object FFields: TListBox
      Left = 120
      Top = 140
      Width = 121
      Height = 125
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 2
      OnClick = FBOkCheckEnabled
    end
    object FParentFields: TListBox
      Left = 256
      Top = 140
      Width = 121
      Height = 125
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 4
      OnClick = FBOkCheckEnabled
    end
    object FTable: TEdit
      Left = 120
      Top = 108
      Width = 121
      Height = 21
      Enabled = False
      TabOrder = 1
      Text = 'FTable'
      OnChange = FTableChange
    end
    object FName: TEdit
      Left = 120
      Top = 16
      Width = 145
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 64
      TabOrder = 0
      Text = 'FName'
      OnChange = FBOkCheckEnabled
    end
    object FDatabase: TEdit
      Left = 120
      Top = 76
      Width = 121
      Height = 21
      Enabled = False
      TabOrder = 5
      Text = 'FDatabase'
    end
    object FParentDatabase: TComboBox_Ext
      Left = 256
      Top = 76
      Width = 121
      Height = 21
      Style = csDropDownList
      TabOrder = 6
      OnChange = FParentDatabaseChange
    end
  end
  object FBHelp: TButton
    Left = 8
    Top = 443
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'FBHelp'
    TabOrder = 0
    OnClick = FBHelpClick
  end
  object GAttributes: TGroupBox_Ext
    Left = 8
    Top = 292
    Width = 389
    Height = 129
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'GAttributes'
    TabOrder = 4
    object FLMatch: TLabel
      Left = 8
      Top = 24
      Width = 42
      Height = 13
      Caption = 'FLMatch'
    end
    object FLOnDelete: TLabel
      Left = 8
      Top = 68
      Width = 57
      Height = 13
      Caption = 'FLOnDelete'
      FocusControl = FOnDelete
    end
    object FLOnUpdate: TLabel
      Left = 8
      Top = 100
      Width = 61
      Height = 13
      Caption = 'FLOnUpdate'
      FocusControl = FOnUpdate
    end
    object FOnDelete: TComboBox_Ext
      Left = 120
      Top = 64
      Width = 121
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      OnChange = FBOkCheckEnabled
    end
    object FOnUpdate: TComboBox_Ext
      Left = 120
      Top = 96
      Width = 121
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnChange = FBOkCheckEnabled
    end
    object FMatchFull: TCheckBox
      Left = 120
      Top = 24
      Width = 73
      Height = 17
      Caption = 'FMatchFull'
      TabOrder = 0
      OnClick = FMatchFullClick
      OnKeyPress = FMatchFullKeyPress
    end
    object FMatchPartial: TCheckBox
      Left = 200
      Top = 24
      Width = 73
      Height = 17
      Caption = 'FMatchPartial'
      TabOrder = 1
      OnClick = FMatchPartialClick
      OnKeyPress = FMatchPartialKeyPress
    end
  end
end
