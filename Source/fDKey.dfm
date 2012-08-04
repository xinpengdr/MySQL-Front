object DIndex: TDIndex
  Left = 713
  Top = 298
  BorderStyle = bsDialog
  Caption = 'DIndex'
  ClientHeight = 441
  ClientWidth = 337
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
    337
    441)
  PixelsPerInch = 106
  TextHeight = 13
  object PSQLWait: TPanel
    Left = 8
    Top = 8
    Width = 321
    Height = 313
    Cursor = crHourGlass
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'PSQLWait'
    TabOrder = 0
    Visible = False
  end
  object FBOk: TButton
    Left = 165
    Top = 408
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object FBCancel: TButton
    Left = 253
    Top = 408
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 5
  end
  object GBasics: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 321
    Height = 338
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'GBasics'
    TabOrder = 1
    DesignSize = (
      321
      338)
    object FLName: TLabel
      Left = 8
      Top = 16
      Width = 40
      Height = 13
      Caption = 'FLName'
    end
    object FLIndexedFields: TLabel
      Left = 8
      Top = 64
      Width = 77
      Height = 13
      Caption = 'FLIndexedFields'
      FocusControl = FIndexedFields
    end
    object FLAvailableFields: TLabel
      Left = 184
      Top = 64
      Width = 82
      Height = 13
      Caption = 'FLAvailableFields'
      FocusControl = FAvailableFields
    end
    object FLLength: TLabel
      Left = 8
      Top = 217
      Width = 45
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'FLLength'
      Enabled = False
      ExplicitTop = 216
    end
    object FLBlockSize: TLabel
      Left = 8
      Top = 271
      Width = 59
      Height = 13
      Caption = 'FLBlockSize'
    end
    object FLComment: TLabel
      Left = 8
      Top = 308
      Width = 56
      Height = 13
      Caption = 'FLComment'
    end
    object FAvailableFields: TListView
      Left = 184
      Top = 80
      Width = 125
      Height = 174
      Anchors = [akLeft, akTop, akBottom]
      Columns = <
        item
          AutoSize = True
          Caption = 'FieldName'
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 4
      ViewStyle = vsReport
      OnChange = FAvailableFieldsChange
      OnDblClick = tbAddOneClick
      OnDeletion = FAvailableFieldsDeletion
      OnEnter = FAvailableFieldsEnter
      OnExit = FAvailableFieldsExit
      ExplicitHeight = 173
    end
    object Panel: TPanel
      Left = 148
      Top = 80
      Width = 23
      Height = 177
      BevelOuter = bvNone
      TabOrder = 5
      object ToolBar1: TToolBar
        Left = 0
        Top = 0
        Width = 23
        Height = 24
        Align = alNone
        TabOrder = 3
        Transparent = False
        object tbAddAll: TToolButton
          Left = 0
          Top = 0
          Caption = 'tbAddAll'
          Enabled = False
          ImageIndex = 47
          OnClick = tbAddAllClick
        end
      end
      object ToolBar2: TToolBar
        Left = 0
        Top = 24
        Width = 23
        Height = 22
        Align = alNone
        AutoSize = True
        TabOrder = 5
        Transparent = False
        object tbAddOne: TToolButton
          Left = 0
          Top = 0
          Caption = 'tbAddOne'
          Enabled = False
          ImageIndex = 48
          OnClick = tbAddOneClick
        end
      end
      object ToolBar3: TToolBar
        Left = 0
        Top = 61
        Width = 23
        Height = 22
        Align = alNone
        AutoSize = True
        TabOrder = 0
        Transparent = False
        object tbUp: TToolButton
          Left = 0
          Top = 0
          Caption = 'tbUp'
          Enabled = False
          ImageIndex = 49
          OnClick = tbUpDownClick
        end
      end
      object ToolBar4: TToolBar
        Left = 0
        Top = 85
        Width = 23
        Height = 22
        Align = alNone
        AutoSize = True
        TabOrder = 1
        Transparent = False
        object tbDown: TToolButton
          Left = 0
          Top = 0
          Caption = 'tbDown'
          Enabled = False
          ImageIndex = 50
          OnClick = tbUpDownClick
        end
      end
      object ToolBar5: TToolBar
        Left = 0
        Top = 123
        Width = 23
        Height = 22
        Align = alNone
        AutoSize = True
        TabOrder = 2
        Transparent = False
        object tbRemoveOne: TToolButton
          Left = 0
          Top = 0
          Caption = 'tbRemoveOne'
          Enabled = False
          ImageIndex = 51
          OnClick = tbRemoveOneClick
        end
      end
      object ToolBar6: TToolBar
        Left = 0
        Top = 147
        Width = 23
        Height = 22
        Align = alNone
        AutoSize = True
        TabOrder = 4
        Transparent = False
        object tbRemoveAll: TToolButton
          Left = 0
          Top = 0
          Caption = 'tbRemoveAll'
          Enabled = False
          ImageIndex = 52
          OnClick = tbRemoveAllClick
        end
      end
    end
    object FPrimary: TRadioButton
      Left = 120
      Top = 16
      Width = 169
      Height = 17
      Caption = 'FPrimary'
      TabOrder = 0
      OnClick = IndexTypeChange
    end
    object FOther: TRadioButton
      Left = 120
      Top = 36
      Width = 17
      Height = 17
      TabOrder = 1
      OnClick = IndexTypeChange
    end
    object FName: TEdit
      Left = 136
      Top = 34
      Width = 145
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 64
      TabOrder = 2
      Text = 'FName'
      OnChange = FNameChange
    end
    object FIndexedFields: TListView
      Left = 8
      Top = 80
      Width = 127
      Height = 130
      Anchors = [akLeft, akTop, akBottom]
      Columns = <
        item
          AutoSize = True
          Caption = 'FieldName'
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 3
      ViewStyle = vsReport
      OnChange = FIndexedFieldsChange
      OnDblClick = tbRemoveOneClick
      OnDeletion = FIndexedFieldsDeletion
      OnEnter = FIndexedFieldsEnter
      OnExit = FIndexedFieldsExit
      ExplicitHeight = 129
    end
    object FLength: TEdit
      Left = 8
      Top = 233
      Width = 41
      Height = 21
      Anchors = [akLeft, akBottom]
      Enabled = False
      TabOrder = 6
      Text = '1'
      OnExit = FLengthExit
      ExplicitTop = 232
    end
    object FLengthUD: TUpDown
      Left = 49
      Top = 233
      Width = 15
      Height = 21
      Anchors = [akLeft, akBottom]
      Associate = FLength
      Enabled = False
      Max = 255
      Position = 1
      TabOrder = 7
      OnExit = FLengthExit
      ExplicitTop = 232
    end
    object FComment: TEdit
      Left = 136
      Top = 305
      Width = 173
      Height = 21
      TabOrder = 10
      Text = 'FComment'
      OnChange = FBOkCheckEnabled
    end
    object FBlockSize: TEdit
      Left = 136
      Top = 268
      Width = 49
      Height = 21
      TabOrder = 8
      Text = '0'
      OnChange = FBOkCheckEnabled
    end
    object FBlockSizeUD: TUpDown
      Left = 185
      Top = 268
      Width = 16
      Height = 21
      Associate = FBlockSize
      TabOrder = 9
    end
  end
  object GAttributes: TGroupBox_Ext
    Left = 8
    Top = 350
    Width = 321
    Height = 41
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'GAttributes'
    TabOrder = 2
    object FFulltext: TCheckBox
      Left = 160
      Top = 16
      Width = 145
      Height = 17
      Caption = 'FFulltext'
      TabOrder = 1
      OnClick = FFulltextClick
    end
    object FUnique: TCheckBox
      Left = 8
      Top = 16
      Width = 145
      Height = 17
      Caption = 'FUnique'
      TabOrder = 0
      OnClick = FUniqueClick
    end
  end
  object FBHelp: TButton
    Left = 8
    Top = 408
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'FBHelp'
    TabOrder = 3
    OnClick = FBHelpClick
  end
end
