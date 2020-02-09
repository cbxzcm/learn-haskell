>1. What functions are being imported from Control.Monad?

forever, when

>2. Which imports are both unqualified and imported in their entirety?

Data.Bits, Database.Blacktip.Types

>3. From the name, what do you suppose importing blacktip’s Types module brings in?

New types that are specific to Blacktip.

>4a. The type signature refers to three aliased imports. What modules are named in those aliases?

Control.Concurrent.MVar, Filesystem.Path.CurrentOS, Control.Concurrent

>4b. Which import does FS.writeFile refer to?

import qualified Filesystem as FS

>4c. Which import did forever come from?

import Control.Monad (forever, when)