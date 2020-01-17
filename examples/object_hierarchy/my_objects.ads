with Aof.Core.Objects;
with Widgets;
with Labels;

package My_Objects is
   
   Top               : aliased Widgets.Widget;
   Form              : aliased Widgets.Widget;
   Label             : aliased Labels.Label;
   Row_Column_Layout : aliased Widgets.Widget;
   Ok                : aliased Widgets.Widget;
   Cancel            : aliased Widgets.Widget;
   
   Top_Ptr               : Aof.Core.Objects.Access_Object := Top'Access;
   Form_Ptr              : Aof.Core.Objects.Access_Object := Form'Access;
   Label_Ptr             : Aof.Core.Objects.Access_Object := Label'Access;
   Row_Column_Layout_Ptr : Aof.Core.Objects.Access_Object := Row_Column_Layout'Access;
   Ok_Ptr                : Aof.Core.Objects.Access_Object := Ok'Access;
   Cancel_Ptr            : Aof.Core.Objects.Access_Object := Cancel'Access;
   
end My_Objects;
