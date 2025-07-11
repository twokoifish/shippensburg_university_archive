// Mocks generated by Mockito 5.1.0 from annotations
// in frpg_companion/test/features/login/login_mock.dart.
// Do not manually edit this file.

import 'dart:async' as _i4;

import 'package:frpg_companion/features/login/data/data.dart' as _i3;
import 'package:frpg_companion/features/network/network.dart' as _i2;
import 'package:mockito/mockito.dart' as _i1;

// ignore_for_file: type=lint
// ignore_for_file: avoid_redundant_argument_values
// ignore_for_file: avoid_setters_without_getters
// ignore_for_file: comment_references
// ignore_for_file: implementation_imports
// ignore_for_file: invalid_use_of_visible_for_testing_member
// ignore_for_file: prefer_const_constructors
// ignore_for_file: unnecessary_parenthesis
// ignore_for_file: camel_case_types

class _FakeServiceClient_0 extends _i1.Fake implements _i2.ServiceClient {}

class _FakeLoginWithCredentialsResponse_1 extends _i1.Fake
    implements _i3.LoginWithCredentialsResponse {}

class _FakeLoginWithCredentialsDatasource_2 extends _i1.Fake
    implements _i3.LoginWithCredentialsDatasource {}

class _FakeResult_3<T> extends _i1.Fake implements _i2.Result<T> {}

/// A class which mocks [ServiceClient].
///
/// See the documentation for Mockito's code generation for more information.
class MockServiceClient extends _i1.Mock implements _i2.ServiceClient {
  MockServiceClient() {
    _i1.throwOnMissingStub(this);
  }

  @override
  String get baseURL =>
      (super.noSuchMethod(Invocation.getter(#baseURL), returnValue: '')
          as String);
  @override
  _i4.Future<Map<String, dynamic>> get(
          {String? endpoint, Map<String, String>? headers}) =>
      (super.noSuchMethod(
          Invocation.method(#get, [], {#endpoint: endpoint, #headers: headers}),
          returnValue:
              Future<Map<String, dynamic>>.value(<String, dynamic>{})) as _i4
          .Future<Map<String, dynamic>>);
  @override
  _i4.Future<Map<String, dynamic>> post(
          {String? endpoint,
          Map<String, String>? headers,
          Map<String, dynamic>? body}) =>
      (super.noSuchMethod(
          Invocation.method(
              #post, [], {#endpoint: endpoint, #headers: headers, #body: body}),
          returnValue:
              Future<Map<String, dynamic>>.value(<String, dynamic>{})) as _i4
          .Future<Map<String, dynamic>>);
}

/// A class which mocks [LoginWithCredentialsDatasource].
///
/// See the documentation for Mockito's code generation for more information.
class MockLoginWithCredentialsDatasource extends _i1.Mock
    implements _i3.LoginWithCredentialsDatasource {
  MockLoginWithCredentialsDatasource() {
    _i1.throwOnMissingStub(this);
  }

  @override
  _i2.ServiceClient get sc => (super.noSuchMethod(Invocation.getter(#sc),
      returnValue: _FakeServiceClient_0()) as _i2.ServiceClient);
  @override
  _i4.Future<_i3.LoginWithCredentialsResponse> loginWithCredentials(
          {_i3.LoginWithCredentialsRequest? request}) =>
      (super.noSuchMethod(
              Invocation.method(#loginWithCredentials, [], {#request: request}),
              returnValue: Future<_i3.LoginWithCredentialsResponse>.value(
                  _FakeLoginWithCredentialsResponse_1()))
          as _i4.Future<_i3.LoginWithCredentialsResponse>);
}

/// A class which mocks [LoginRepository].
///
/// See the documentation for Mockito's code generation for more information.
class MockLoginRepository extends _i1.Mock implements _i3.LoginRepository {
  MockLoginRepository() {
    _i1.throwOnMissingStub(this);
  }

  @override
  _i3.LoginWithCredentialsDatasource get loginWithCredentialsDatasource =>
      (super.noSuchMethod(Invocation.getter(#loginWithCredentialsDatasource),
              returnValue: _FakeLoginWithCredentialsDatasource_2())
          as _i3.LoginWithCredentialsDatasource);
  @override
  _i4.Future<_i2.Result<_i3.LoginWithCredentialsResponse>> loginWithCredentials(
          {_i3.LoginWithCredentialsRequest? request}) =>
      (super.noSuchMethod(
              Invocation.method(#loginWithCredentials, [], {#request: request}),
              returnValue:
                  Future<_i2.Result<_i3.LoginWithCredentialsResponse>>.value(
                      _FakeResult_3<_i3.LoginWithCredentialsResponse>()))
          as _i4.Future<_i2.Result<_i3.LoginWithCredentialsResponse>>);
}
