/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 1994 - 2021 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */
package eu.internetofus.wenet_interaction_protocol_engine;

import static eu.internetofus.common.components.AbstractComponentMocker.createClientWithDefaultSession;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.model.ModelTestCase;
import eu.internetofus.common.components.StoreServices;
import eu.internetofus.common.components.models.Incentive;
import eu.internetofus.common.components.models.IncentiveTest;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessageTest;
import eu.internetofus.common.components.models.CommunityProfile;
import eu.internetofus.common.components.models.CommunityProfileTest;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.components.profile_manager.WeNetProfileManagerMocker;
import eu.internetofus.common.components.models.WeNetUserProfile;
import eu.internetofus.common.components.models.WeNetUserProfileTest;
import eu.internetofus.common.components.service.WeNetService;
import eu.internetofus.common.components.service.WeNetServiceSimulator;
import eu.internetofus.common.components.service.WeNetServiceSimulatorMocker;
import eu.internetofus.common.components.models.ProtocolNorm;
import eu.internetofus.common.components.models.Task;
import eu.internetofus.common.components.models.TaskTest;
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.components.models.TaskTransactionTest;
import eu.internetofus.common.components.models.TaskType;
import eu.internetofus.common.components.models.TaskTypeTest;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.common.components.task_manager.WeNetTaskManagerMocker;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.UUID;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test the {@link ProtocolData}
 *
 * @see ProtocolData
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(VertxExtension.class)
public class ProtocolDataTest extends ModelTestCase<ProtocolData> {

  /**
   * The profile manager mocked server.
   */
  protected static WeNetProfileManagerMocker profileManagerMocker;

  /**
   * The task manager mocked server.
   */
  protected static WeNetTaskManagerMocker taskManagerMocker;

  /**
   * The service mocked server.
   */
  protected static WeNetServiceSimulatorMocker serviceMocker;

  /**
   * Start the mocker server.
   */
  @BeforeAll
  public static void startMockers() {

    profileManagerMocker = WeNetProfileManagerMocker.start();
    taskManagerMocker = WeNetTaskManagerMocker.start();
    serviceMocker = WeNetServiceSimulatorMocker.start();
  }

  /**
   * Stop the mocker server.
   */
  @AfterAll
  public static void stopMockers() {

    profileManagerMocker.stopServer();
    taskManagerMocker.stopServer();
    serviceMocker.stopServer();
  }

  /**
   * Register the necessary services before to test.
   *
   * @param vertx event bus to register the necessary services.
   */
  @BeforeEach
  public void registerServices(final Vertx vertx) {

    final var client = createClientWithDefaultSession(vertx);
    final var profileConf = profileManagerMocker.getComponentConfiguration();
    WeNetProfileManager.register(vertx, client, profileConf);

    final var taskConf = taskManagerMocker.getComponentConfiguration();
    WeNetTaskManager.register(vertx, client, taskConf);

    final var conf = serviceMocker.getComponentConfiguration();
    WeNetService.register(vertx, client, conf);
    WeNetServiceSimulator.register(vertx, client, conf);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ProtocolData createModelExample(final int index) {

    final var model = new ProtocolData();
    model.task = new TaskTest().createModelExample(index);
    model.taskType = new TaskTypeTest().createModelExample(index);
    model.community = new CommunityProfileTest().createModelExample(index);
    model.profile = new WeNetUserProfileTest().createModelExample(index);
    return model;
  }

  /**
   * Check that create empty protocol with {@code null} identifiers.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#createWith(String, String, String, String, Vertx)
   */
  @Test
  public void shouldCreateEmptyDataWithNullIdentifiers(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertComplete(ProtocolData.createWith(null, null, null, null, vertx))
        .onSuccess(protocol -> testContext.verify(() -> {

          assertThat(protocol).isEqualTo(new ProtocolData());
          testContext.completeNow();

        }));

  }

  /**
   * Check that create empty protocol with undefined identifiers.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#createWith(String, String, String, String, Vertx)
   */
  @Test
  public void shouldCreateEmptyDataWithUndefinedIdentifiers(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertComplete(ProtocolData.createWith("undefined", "undefined", "undefined", "undefined", vertx))
        .onSuccess(protocol -> testContext.verify(() -> {

          assertThat(protocol).isEqualTo(new ProtocolData());
          testContext.completeNow();

        }));

  }

  /**
   * Check that protocol with community.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#createWith(String, String, String, String, Vertx)
   */
  @Test
  public void shouldCreateWithCommunity(final Vertx vertx, final VertxTestContext testContext) {

    StoreServices.storeCommunityExample(1, vertx, testContext).onSuccess(community -> {
      testContext.assertComplete(ProtocolData.createWith(null, null, community.id, null, vertx))
          .onSuccess(protocol -> testContext.verify(() -> {

            final var expected = new ProtocolData();
            expected.community = community;
            assertThat(protocol).isEqualTo(expected);
            testContext.completeNow();

          }));

    });

  }

  /**
   * Check that protocol with profile.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#createWith(String, String, String, String, Vertx)
   */
  @Test
  public void shouldCreateWithProfile(final Vertx vertx, final VertxTestContext testContext) {

    StoreServices.storeProfileExample(1, vertx, testContext).onSuccess(profile -> {
      testContext.assertComplete(ProtocolData.createWith(null, null, null, profile.id, vertx))
          .onSuccess(protocol -> testContext.verify(() -> {

            final var expected = new ProtocolData();
            expected.profile = profile;
            assertThat(protocol).isEqualTo(expected);
            testContext.completeNow();

          }));

    });

  }

  /**
   * Check that protocol with task.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#createWith(String, String, String, String, Vertx)
   * @see ProtocolData#createWith(eu.internetofus.common.components.task_manager.Task,
   *      Vertx)
   */
  @Test
  public void shouldCreateWithTask(final Vertx vertx, final VertxTestContext testContext) {

    StoreServices.storeTaskExample(1, vertx, testContext).onSuccess(task -> {
      testContext.assertComplete(ProtocolData.createWith(task.id, null, null, null, vertx))
          .onSuccess(protocol -> testContext.verify(() -> {

            assertThat(protocol.task).isEqualTo(task);
            assertThat(protocol.taskType).isNotNull();
            assertThat(protocol.taskType.id).isEqualTo(task.taskTypeId);
            assertThat(protocol.community).isNotNull();
            assertThat(protocol.community.id).isEqualTo(task.communityId);
            assertThat(protocol.profile).isNotNull();
            assertThat(protocol.profile.id).isEqualTo(task.requesterId);
            testContext.assertComplete(ProtocolData.createWith(task, vertx))
                .onSuccess(protocolWithTask -> testContext.verify(() -> {

                  assertThat(protocol).isEqualTo(protocolWithTask);
                  testContext.completeNow();

                }));

          }));

    });

  }

  /**
   * Check that protocol with task transaction.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#createWith(String, String, String, String, Vertx)
   * @see ProtocolData#createWith(eu.internetofus.common.components.task_manager.TaskTransaction,
   *      Vertx)
   */
  @Test
  public void shouldCreateWithTaskTransaction(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertComplete(new TaskTransactionTest().createModelExample(1, vertx, testContext))
        .onSuccess(taskTransaction -> {
          testContext
              .assertComplete(
                  ProtocolData.createWith(taskTransaction.taskId, null, null, taskTransaction.actioneerId, vertx))
              .onSuccess(protocol -> testContext.verify(() -> {

                assertThat(protocol.task).isNotNull();
                assertThat(protocol.task.id).isEqualTo(taskTransaction.taskId);
                assertThat(protocol.taskType).isNotNull();
                assertThat(protocol.community).isNotNull();
                assertThat(protocol.profile).isNotNull();
                assertThat(protocol.profile.id).isEqualTo(taskTransaction.actioneerId);
                testContext.assertComplete(ProtocolData.createWith(taskTransaction, vertx))
                    .onSuccess(protocolWithTaskTransaction -> testContext.verify(() -> {

                      assertThat(protocol).isEqualTo(protocolWithTaskTransaction);
                      testContext.completeNow();

                    }));

              }));

        });

  }

  /**
   * Check is the protocol has norms.
   *
   * @see ProtocolData#hasProtocolNorms()
   */
  @Test
  public void shouldCheckIfTheprotocolHasNorms() {

    final var protocol = new ProtocolData();
    assertThat(protocol.hasProtocolNorms()).isFalse();

    protocol.community = new CommunityProfile();
    assertThat(protocol.hasProtocolNorms()).isFalse();

    protocol.community.norms = new ArrayList<>();
    assertThat(protocol.hasProtocolNorms()).isFalse();

    protocol.community.norms.add(new ProtocolNorm());
    assertThat(protocol.hasProtocolNorms()).isTrue();

    protocol.community = null;
    protocol.taskType = new TaskType();
    assertThat(protocol.hasProtocolNorms()).isFalse();

    protocol.taskType.norms = new ArrayList<>();
    assertThat(protocol.hasProtocolNorms()).isFalse();

    protocol.taskType.norms.add(new ProtocolNorm());
    assertThat(protocol.hasProtocolNorms()).isTrue();

  }

  /**
   * Remove task if has different identifier.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#loadTaskIn(String, ProtocolData, Vertx)
   */
  @Test
  public void shouldRemoveTaskIfIdIsDiferrent(final Vertx vertx, final VertxTestContext testContext) {

    final var protocol = new ProtocolData();
    protocol.task = new Task();
    protocol.task.id = UUID.randomUUID().toString();
    testContext.assertComplete(ProtocolData.loadTaskIn(UUID.randomUUID().toString(), protocol, vertx))
        .onSuccess(newProtocol -> testContext.verify(() -> {
          assertThat(newProtocol.task).isNull();
          testContext.completeNow();
        }));

  }

  /**
   * Remove task type if has different identifier.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#loadTaskTypeIn(String, ProtocolData, Vertx)
   */
  @Test
  public void shouldRemoveTaskTypeIfIdIsDiferrent(final Vertx vertx, final VertxTestContext testContext) {

    final var protocol = new ProtocolData();
    protocol.taskType = new TaskType();
    protocol.taskType.id = UUID.randomUUID().toString();
    testContext.assertComplete(ProtocolData.loadTaskTypeIn(UUID.randomUUID().toString(), protocol, vertx))
        .onSuccess(newProtocol -> testContext.verify(() -> {
          assertThat(newProtocol.taskType).isNull();
          testContext.completeNow();
        }));

  }

  /**
   * Remove community if has different identifier.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#loadCommunityIn(String, ProtocolData, Vertx)
   */
  @Test
  public void shouldRemoveCommunityIfIdIsDiferrent(final Vertx vertx, final VertxTestContext testContext) {

    final var protocol = new ProtocolData();
    protocol.community = new CommunityProfile();
    protocol.community.id = UUID.randomUUID().toString();
    testContext.assertComplete(ProtocolData.loadCommunityIn(UUID.randomUUID().toString(), protocol, vertx))
        .onSuccess(newProtocol -> testContext.verify(() -> {
          assertThat(newProtocol.community).isNull();
          testContext.completeNow();
        }));

  }

  /**
   * Remove profile if has different identifier.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#loadProfileIn(String, ProtocolData, Vertx)
   */
  @Test
  public void shouldRemoveProfileIfIdIsDiferrent(final Vertx vertx, final VertxTestContext testContext) {

    final var protocol = new ProtocolData();
    protocol.profile = new WeNetUserProfile();
    protocol.profile.id = UUID.randomUUID().toString();
    testContext.assertComplete(ProtocolData.loadProfileIn(UUID.randomUUID().toString(), protocol, vertx))
        .onSuccess(newProtocol -> testContext.verify(() -> {
          assertThat(newProtocol.profile).isNull();
          testContext.completeNow();
        }));

  }

  /**
   * Check that create empty protocol with empty task.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#createWith(Task, Vertx)
   */
  @Test
  public void shouldCreateEmptyDataWithEmptyTask(final Vertx vertx, final VertxTestContext testContext) {

    final var task = new Task();
    testContext.assertComplete(ProtocolData.createWith(task, vertx)).onSuccess(protocol -> testContext.verify(() -> {

      final var expected = new ProtocolData();
      expected.task = task;
      assertThat(protocol).isEqualTo(expected);
      testContext.completeNow();

    }));

  }

  /**
   * Check that create empty protocol with empty task transaction.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#createWith(TaskTransaction, Vertx)
   */
  @Test
  public void shouldCreateEmptyDataWithEmptyTaskTransaction(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertComplete(ProtocolData.createWith(new TaskTransaction(), vertx))
        .onSuccess(protocol -> testContext.verify(() -> {

          assertThat(protocol).isEqualTo(new ProtocolData());
          testContext.completeNow();

        }));

  }

  /**
   * Check that create empty protocol with empty incentive.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#createWith(Incentive, Vertx)
   */
  @Test
  public void shouldCreateEmptyDataWithEmptyIncentive(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertComplete(ProtocolData.createWith(new Incentive(), vertx))
        .onSuccess(protocol -> testContext.verify(() -> {

          assertThat(protocol).isEqualTo(new ProtocolData());
          testContext.completeNow();

        }));

  }

  /**
   * Check that create protocol with incentive.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#createWith(Incentive, Vertx)
   */
  @Test
  public void shouldCreateProtocolDataWithIncentive(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertComplete(new IncentiveTest().createModelExample(1, vertx, testContext)).onSuccess(incentive -> {

      testContext.assertComplete(ProtocolData.createWith(incentive, vertx))
          .onSuccess(protocol -> testContext.verify(() -> {

            assertThat(protocol).isNotNull();
            assertThat(protocol.community).isNotNull();
            assertThat(protocol.profile).isNotNull();
            assertThat(protocol.profile.id).isEqualTo(incentive.UserId);
            testContext.completeNow();

          }));

    });

  }

  /**
   * Check that create empty protocol with empty protocol message.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#createWith(Incentive, Vertx)
   */
  @Test
  public void shouldCreateEmptyDataWithEmptyProtocolMessage(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertComplete(ProtocolData.createWith(new ProtocolMessage(), vertx))
        .onSuccess(protocol -> testContext.verify(() -> {

          assertThat(protocol).isEqualTo(new ProtocolData());
          testContext.completeNow();

        }));

  }

  /**
   * Check that create protocol with incentive.
   *
   * @param vertx       event bus to use.
   * @param testContext test context to use.
   *
   * @see ProtocolData#createWith(Incentive, Vertx)
   */
  @Test
  public void shouldCreateProtocolDataWithProtocolMesssage(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertComplete(new ProtocolMessageTest().createModelExample(1, vertx, testContext))
        .onSuccess(message -> {

          testContext.assertComplete(ProtocolData.createWith(message, vertx))
              .onSuccess(protocol -> testContext.verify(() -> {

                assertThat(protocol).isNotNull();
                assertThat(protocol.task).isNotNull();
                assertThat(protocol.task.id).isEqualTo(message.taskId);
                assertThat(protocol.taskType).isNotNull();
                assertThat(protocol.taskType.id).isEqualTo(protocol.task.taskTypeId);
                assertThat(protocol.community).isNotNull();
                assertThat(protocol.community.id).isEqualTo(message.communityId);
                assertThat(protocol.profile).isNotNull();
                assertThat(protocol.profile.id).isEqualTo(message.receiver.userId);
                testContext.completeNow();

              }));

        });

  }
}
