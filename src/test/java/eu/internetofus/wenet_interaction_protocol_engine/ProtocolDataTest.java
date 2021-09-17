/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */
package eu.internetofus.wenet_interaction_protocol_engine;

import static eu.internetofus.common.components.AbstractComponentMocker.createClientWithDefaultSession;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessageTest;
import eu.internetofus.common.components.models.CommunityProfile;
import eu.internetofus.common.components.models.CommunityProfileTest;
import eu.internetofus.common.components.models.Incentive;
import eu.internetofus.common.components.models.IncentiveTest;
import eu.internetofus.common.components.models.ProtocolNorm;
import eu.internetofus.common.components.models.Task;
import eu.internetofus.common.components.models.TaskTest;
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.components.models.TaskType;
import eu.internetofus.common.components.models.TaskTypeTest;
import eu.internetofus.common.components.models.WeNetUserProfile;
import eu.internetofus.common.components.models.WeNetUserProfileTest;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.components.profile_manager.WeNetProfileManagerMocker;
import eu.internetofus.common.components.service.WeNetService;
import eu.internetofus.common.components.service.WeNetServiceSimulator;
import eu.internetofus.common.components.service.WeNetServiceSimulatorMocker;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.common.components.task_manager.WeNetTaskManagerMocker;
import eu.internetofus.common.model.ModelTestCase;
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
