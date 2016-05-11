# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Copyright 2016 and onwards Google, Inc.
#
# See www.openfst.org for extensive documentation on this weighted
# finite-state transducer library.


from libcpp.memory cimport shared_ptr


# This is mysteriously missing from libcpp.memory.

cdef extern from "<memory>" namespace "std" nogil:

  shared_ptr[T] static_pointer_cast[T, U](const shared_ptr[U] &)

